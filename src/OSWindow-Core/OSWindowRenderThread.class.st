"
I am a pharo process that performs all rendering calls. 
"
Class {
	#name : #OSWindowRenderThread,
	#superclass : #Object,
	#instVars : [
		'renderThreadQueue',
		'operationAvailableSemaphore',
		'registeredWindows',
		'registeredWindowsMutex',
		'activeRenderThreadProcessMutex',
		'hasAnimatedWindowsInThisPeriod'
	],
	#classVars : [
		'ActiveRenderThreadProcess',
		'RenderSession',
		'RenderThreadProcess',
		'UniqueInstance'
	],
	#category : #'OSWindow-Core-Extras'
}

{ #category : #'window animation' }
OSWindowRenderThread class >> animationStep [
	^ 10
]

{ #category : #'session management' }
OSWindowRenderThread class >> checkSession [
	RenderSession ~~ Smalltalk session ifTrue: [ 
		UniqueInstance := self new.
	]

]

{ #category : #'window animation' }
OSWindowRenderThread class >> createdWindow: window [
	^ self uniqueInstance createdWindow: window
]

{ #category : #'window animation' }
OSWindowRenderThread class >> destroyedWindow: window [
	^ self uniqueInstance destroyedWindow: window
]

{ #category : #'window animation' }
OSWindowRenderThread class >> doAnimationFrameFor: window [
	"TODO: Handle errors produced here"
	(OSWindowFrameEvent for: window) deliver
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueBlockingOperation: operation [
	^ self uniqueInstance enqueueBlockingOperation: operation
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation [
	^ self uniqueInstance enqueueOperation: operation blocking: false
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation blocking: blocking [
	^ self uniqueInstance enqueueOperation: operation blocking: blocking
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation blocking: blocking ifError: aBlock [
	^ self uniqueInstance enqueueOperation: operation blocking: blocking ifError: aBlock
]

{ #category : #'render thread' }
OSWindowRenderThread class >> isActiveThread [
	^ self uniqueInstance isActiveThread
]

{ #category : #'render thread' }
OSWindowRenderThread class >> isThisThread [
	^ self uniqueInstance isThisThread
]

{ #category : #'session management' }
OSWindowRenderThread class >> uniqueInstance [
	self checkSession.
	^ UniqueInstance

]

{ #category : #'window animation' }
OSWindowRenderThread >> animationStep [
	^ 10
]

{ #category : #'window animation' }
OSWindowRenderThread >> createRenderThread [
	RenderThreadProcess ifNotNil: [ RenderThreadProcess terminate ].
	ActiveRenderThreadProcess := nil.
	
	renderThreadQueue := AtomicSharedQueue new.
	operationAvailableSemaphore := Semaphore new.
	
	registeredWindows := WeakSet new.
	registeredWindowsMutex := Semaphore forMutualExclusion.
	
	activeRenderThreadProcessMutex := Mutex new.
	
	RenderThreadProcess := [ self renderThreadProcess ] forkAt: Processor userSchedulingPriority.
	RenderThreadProcess name:  'OS Window Renderer Loop'; resume.

]

{ #category : #'window animation' }
OSWindowRenderThread >> createdWindow: window [
	registeredWindowsMutex critical: [
		registeredWindows add: window.
		operationAvailableSemaphore signal
	]

]

{ #category : #'mutual exclusion' }
OSWindowRenderThread >> critical: aBlock [
	| oldActiveRenderThreadProcess |
	activeRenderThreadProcessMutex critical: [
		oldActiveRenderThreadProcess := ActiveRenderThreadProcess.
		ActiveRenderThreadProcess := Processor activeProcess.
		[
			aBlock value
		] ensure: [ ActiveRenderThreadProcess := oldActiveRenderThreadProcess ]
	]
]

{ #category : #'window animation' }
OSWindowRenderThread >> destroyedWindow: window [
	registeredWindowsMutex critical:  [ 
		registeredWindows remove: window ifAbsent: [ nil ]
	]
]

{ #category : #'window animation' }
OSWindowRenderThread >> doAnimationFrameFor: window [
	[
		(OSWindowFrameEvent for: window) deliver
	] on: Error do: [ :error |
		"It is critical, that the animation render thread keeps running despite errors."
		error freeze.
		[ error debug ] fork
	]
	
]

{ #category : #'render thread' }
OSWindowRenderThread >> enqueueBlockingOperation: operation [
	^ self enqueueOperation: operation blocking: true
]

{ #category : #'render thread' }
OSWindowRenderThread >> enqueueOperation: operation [
	^ self enqueueOperation: operation blocking: false
]

{ #category : #'render thread' }
OSWindowRenderThread >> enqueueOperation: operation blocking: blocking [
	^ self enqueueOperation: operation blocking: blocking ifError: [ ]
]

{ #category : #'render thread' }
OSWindowRenderThread >> enqueueOperation: operation blocking: blocking ifError: aBlock [
	" If this is in the RenderThread, execute directly"
	| operationHandle |
	Processor activeProcess == RenderThreadProcess ifTrue: [ ^ operation value ].
	
	"Enqueue the operation"
	operationHandle := OSWindowRenderThreadOperation new: operation blocking: blocking.
	renderThreadQueue nextPut: operationHandle.
	operationAvailableSemaphore signal.

	"Wait for blocking operations."
	blocking ifTrue: [
		operationHandle wait.
		operationHandle failed ifTrue: aBlock
	].
]

{ #category : #'render thread' }
OSWindowRenderThread >> executeRenderOperations [
	| operation  |
	[ operation := renderThreadQueue nextOrNil.
	  operation  ~~ nil ] whileTrue: [
		operation execute.
	].

]

{ #category : #initialization }
OSWindowRenderThread >> initialize [
	super initialize.
	self createRenderThread.
	RenderSession := Smalltalk session.

]

{ #category : #initialization }
OSWindowRenderThread >> isActiveThread [
	^ Processor activeProcess == ActiveRenderThreadProcess

]

{ #category : #initialization }
OSWindowRenderThread >> isThisThread [
	^ Processor activeProcess == RenderThreadProcess

]

{ #category : #'window animation' }
OSWindowRenderThread >> performAnimationFrames [
	| windows |
	
	" Copy the registered windows. "
	registeredWindowsMutex critical: [ 
		windows := registeredWindows copy.
	].

	" Try to animate them. "
	hasAnimatedWindowsInThisPeriod := false.
	windows do: [ :window |
		(window isValid) ifTrue: [ 
			hasAnimatedWindowsInThisPeriod := true.
			self doAnimationFrameFor: window.
		]
	].

]

{ #category : #'render thread' }
OSWindowRenderThread >> renderThreadProcess [
	| session |
	session := Smalltalk session.
	
	[session == Smalltalk session] whileTrue: [
		"Execute the enqueued rendering operations"
		operationAvailableSemaphore consumeAllSignals.
		self critical: [
			self executeRenderOperations.
		
			"Animations must be refreshed periodically"
			self performAnimationFrames
		].
	
		self waitMoreOperations
	]



]

{ #category : #'window animation' }
OSWindowRenderThread >> waitMoreOperations [
	"Wait for another operation event or a time out."
	hasAnimatedWindowsInThisPeriod ifTrue:  [
		"TODO: Compute a better time for the Delay. Thedelay is required to give time to threads with a lower priority."
		(Delay forMilliseconds: self animationStep) wait.
		operationAvailableSemaphore waitTimeoutMSecs: self animationStep.
	] ifFalse: [ 
		operationAvailableSemaphore wait.
	]
]
