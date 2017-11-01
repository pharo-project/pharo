"
I am a pharo process that performs all rendering calls. 
"
Class {
	#name : #OSWindowRenderThread,
	#superclass : #Object,
	#classVars : [
		'OperationAvailableSemaphore',
		'RegisteredWindows',
		'RegisteredWindowsMutex',
		'RenderSession',
		'RenderThreadProcess',
		'RenderThreadQueue'
	],
	#category : 'OSWindow-Core-Extras'
}

{ #category : #'window animation' }
OSWindowRenderThread class >> animationStep [
	^ 10
]

{ #category : #'session management' }
OSWindowRenderThread class >> checkSession [
	RenderSession ~~ Smalltalk session ifTrue: [ 
		self createRenderThread.
		RenderSession := Smalltalk session.
	]

]

{ #category : #'render thread' }
OSWindowRenderThread class >> createRenderThread [
	RenderThreadProcess ifNotNil: [ RenderThreadProcess terminate ].
	
	RenderThreadQueue := AtomicSharedQueue new.
	OperationAvailableSemaphore := Semaphore new.
	
	RegisteredWindows := WeakSet new.
	RegisteredWindowsMutex := Semaphore forMutualExclusion.
	
	RenderThreadProcess := [ self renderThreadProcess ] forkAt: Processor userSchedulingPriority.
	RenderThreadProcess name:  'OS Window Renderer Loop'; resume.

]

{ #category : #'window animation' }
OSWindowRenderThread class >> createdWindow: window [
	self checkSession.
	RegisteredWindowsMutex critical: [
		RegisteredWindows add: window
	]

]

{ #category : #'window animation' }
OSWindowRenderThread class >> destroyedWindow: window [
	self checkSession.
	RegisteredWindowsMutex critical:  [ 
		RegisteredWindows remove: window ifAbsent: [ nil ]
	]
]

{ #category : #'window animation' }
OSWindowRenderThread class >> doAnimationFrameFor: window [
	"TODO: Handle errors produced here"
	(OSWindowFrameEvent for: window) deliver
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueBlockingOperation: operation [
	self enqueueOperation: operation blocking: true
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation [
	self enqueueOperation: operation blocking: false
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation blocking: blocking [
	^ self enqueueOperation: operation blocking: blocking ifError: [ ]
]

{ #category : #'render thread' }
OSWindowRenderThread class >> enqueueOperation: operation blocking: blocking ifError: aBlock [
	" If this is in the RenderThread, execute directly"
	| operationHandle |
	Processor activeProcess == RenderThreadProcess ifTrue: [ operation value ].
	
	"Enqueue for execution in the render thread."
	self checkSession.
	
	"Enqueue the operation"
	operationHandle := OSWindowRenderThreadOperation new: operation blocking: blocking.
	RenderThreadQueue nextPut: operationHandle.
	OperationAvailableSemaphore signal.

	"Wait for blocking operations."
	blocking ifTrue: [
		operationHandle wait.
		operationHandle failed ifTrue: aBlock
	].

]

{ #category : #'render thread' }
OSWindowRenderThread class >> executeRenderOperations [
	| operation  |
	[ operation := RenderThreadQueue nextOrNil.
	  operation  ~~ nil ] whileTrue: [
		operation execute.
	].

]

{ #category : #'render thread' }
OSWindowRenderThread class >> isThisThread [
	^ Processor activeProcess == RenderThreadProcess


]

{ #category : #'window animation' }
OSWindowRenderThread class >> performAnimationFrames [
	| windows hasAnimated |
	
	" Copy the registered windows. "
	RegisteredWindowsMutex critical: [ 
		windows := RegisteredWindows copy.
	].

	" Try to animate them. "
	hasAnimated := false.
	windows do: [ :window |
		(window isValid) ifTrue: [ 
			hasAnimated := true.
			self doAnimationFrameFor: window.
		]
	].

	"Wait for another operation event or a time out."
	hasAnimated ifTrue:  [
		"TODO: Compute a better time for the Delay. Thedelay is required to give time to threads with a lower priority."
		(Delay forMilliseconds: self animationStep) wait.
		OperationAvailableSemaphore waitTimeoutMSecs: self animationStep.
	] ifFalse: [ 
		OperationAvailableSemaphore wait.
	]
]

{ #category : #'render thread' }
OSWindowRenderThread class >> renderThreadProcess [
	| session |
	session := Smalltalk session.
	
	[session == Smalltalk session] whileTrue: [
		"Execute the enqueued rendering operations"
		OperationAvailableSemaphore consumeAllSignals.
		self executeRenderOperations.
		
		"Animations must be refreshed periodically"
		self performAnimationFrames
	]



]
