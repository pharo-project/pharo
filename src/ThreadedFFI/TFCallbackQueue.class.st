"
I'm a callback queue associated to a worker.
I handle all callbacks defined in the context of a worker.
"
Class {
	#name : #TFCallbackQueue,
	#superclass : #Object,
	#instVars : [
		'semaphore',
		'callbacksByAddress',
		'pendingQueue',
		'callbackProcess',
		'mutex'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : #'ThreadedFFI-Callbacks'
}

{ #category : #'instance creation' }
TFCallbackQueue class >> initialize [

	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 70.	
		
	self startUp: true
]

{ #category : #'instance creation' }
TFCallbackQueue class >> shutDown: inANewImageSession [

	inANewImageSession ifFalse: [ ^ self ].
	self uniqueInstance shutDown.
	UniqueInstance := nil
]

{ #category : #'instance creation' }
TFCallbackQueue class >> startUp: inANewImageSession [

	inANewImageSession ifFalse: [ ^ self ].
	self uniqueInstance startUp
]

{ #category : #'instance creation' }
TFCallbackQueue class >> uniqueInstance [ 
	
	^ UniqueInstance ifNil: [ UniqueInstance := self new ]
]

{ #category : #accessing }
TFCallbackQueue >> callbackProcess [
	^ callbackProcess
]

{ #category : #operations }
TFCallbackQueue >> executeCallback: aCallbackInvocation [
	
	aCallbackInvocation ifNil: [ ^self ].
	aCallbackInvocation getHandle isNull ifTrue: [ ^self ].
	
	aCallbackInvocation callback: ((self lookupCallback: aCallbackInvocation callbackData) 
		ifNil: [
			Stdio stdout << 'INV: ' << aCallbackInvocation getHandle printString; lf.
			self error: 'Callback not found.' ]).
	aCallbackInvocation runner executeCallback: aCallbackInvocation
]

{ #category : #initialization }
TFCallbackQueue >> forkCallbackProcess [
	
	(callbackProcess notNil and: [ callbackProcess isTerminated not ])
		ifTrue: [ ^ self ].

	callbackProcess := [
		[ true ] whileTrue: [
			semaphore wait.
			[self executeCallback: self nextPendingCallback]
				on: Exception fork:  [:ex | ex pass ] ] ] 
		forkAt: Processor highIOPriority
		named: 'Callback queue'
]

{ #category : #initialization }
TFCallbackQueue >> initialize [
	super initialize.
	
	mutex := Mutex new.
	semaphore := Semaphore new.
	callbacksByAddress := WeakValueDictionary new.
	pendingQueue := OrderedCollection new.
	self forkCallbackProcess
]

{ #category : #initialization }
TFCallbackQueue >> initializeQueue [
	
	| semaphoreIndex |
	
	[	semaphoreIndex := Smalltalk registerExternalObject: semaphore.
		self primitiveInitializeQueueWith: semaphoreIndex ] 
			onErrorDo: [ :e |
				semaphoreIndex ifNotNil: [ Smalltalk unregisterExternalObject: semaphoreIndex ].
				e pass]
]

{ #category : #private }
TFCallbackQueue >> lookupCallback: anExternalAddress [
	"#registerCallback: and #lookupCallback: are protected by a mutex because I need to prevent 
	 the execution of a collection grow while performing the lookup (this will move the elements 
	 and may cause the lookup to fail).
	 Still, I am keeping a second lookup to ensure if a movement was done, I have another oportunity 
	 to find the callback (just in case, I was not able to verify is actually needed)."

	^ mutex critical: [ 
		callbacksByAddress 
			at: anExternalAddress 
			ifAbsent: [ 
				callbacksByAddress 
					at: anExternalAddress
					ifAbsent: [ nil ] ] ]
]

{ #category : #private }
TFCallbackQueue >> nextPendingCallback [
	| externalAddress |
	
	externalAddress := self primNextPendingCallback.
	externalAddress ifNil: [ ^ nil ].
	
	^ TFCallbackInvocation fromHandle: externalAddress
]

{ #category : #'private - primitives' }
TFCallbackQueue >> primNextPendingCallback [
	<primitive: 'primitiveReadNextCallback'>

	^ self primitiveFailed
]

{ #category : #primitives }
TFCallbackQueue >> primitiveInitializeQueueWith: anInteger [ 

	<primitive: 'primitiveInitilizeCallbacks'>

	^ self primitiveFailed 
]

{ #category : #operations }
TFCallbackQueue >> registerCallback: aCallback [
	"#registerCallback: and #lookupCallback: are protected by a mutex because I need to prevent 
	 the execution of a collection grow while performing the lookup (this will move the elements 
	 and may cause the lookup to fail)."

	mutex critical: [ 
		callbacksByAddress 
			at: aCallback callbackData 
			put: aCallback ]
]

{ #category : #'system startup' }
TFCallbackQueue >> shutDown [

	self terminateProcess.
	callbacksByAddress removeAll.
	pendingQueue removeAll
]

{ #category : #'system startup' }
TFCallbackQueue >> startUp [

	[self initializeQueue.
	self forkCallbackProcess] 
		onErrorDo: [ self inform: 'The current VM does not support TFFI Callbacks. It will use the old implementation' ]
]

{ #category : #initialization }
TFCallbackQueue >> terminateProcess [
	
	callbackProcess ifNil: [ ^ self ].
	callbackProcess terminate. 
	callbackProcess := nil
]
