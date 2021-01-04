"
When executing a callout or a callback the invocation pass all through a runner.
There are different kinds of runners.
Each has different policies to run the FFI calls and callbacks.
"
Class {
	#name : #TFRunner,
	#superclass : #FFIExternalReference,
	#instVars : [
		'callbackInvocationStack',
		'stackProtect',
		'exceptionHandler',
		'semaphorePool',
		'callbackReturnSemaphore'
	],
	#category : #'ThreadedFFI-Worker'
}

{ #category : #errors }
TFRunner >> cannotReturnCallbackFromOldSession: aCallbackInvocation [

	^ TFInvalidSessionCallbackReturn new
		callbackInvocation: aCallbackInvocation;
		signal
]

{ #category : #errors }
TFRunner >> cannotReturnCallbackInWrongOrder: aCallbackInvocation [

	^ TFIncorrectOrderCallbackReturn new
		callbackInvocation: aCallbackInvocation;
		previousCallbacks: (callbackInvocationStack copyUpTo: aCallbackInvocation);
		signal
]

{ #category : #executing }
TFRunner >> doInitialize [

	callbackInvocationStack := Stack new.
	stackProtect := Semaphore forMutualExclusion.
	semaphorePool ifNotNil: [ semaphorePool release ].
	semaphorePool := self newSemaphorePool
]

{ #category : #executing }
TFRunner >> ensureInitialized [
	"Only initialize if the image has been restarted and thus I have no handle"
	self isNull ifTrue: [ self doInitialize ]
]

{ #category : #errors }
TFRunner >> exceptionHandler [

	^ exceptionHandler ifNil: [ 
		exceptionHandler := TFForkCallbackExceptionHandler new ]
]

{ #category : #errors }
TFRunner >> exceptionHandler: anExceptionHandler [ 
	
	exceptionHandler := anExceptionHandler
]

{ #category : #executing }
TFRunner >> executeCallback: aCallbackInvocation [
	"Entry point to execute a callback invocation.
	Runs the callback in a separate green thread and stack it.
	The stack gives information about the order in which callbacks should return.
	If a callback does not return in the right order, an exception is thrown.
	
	To guarantee the stack is not modified while this method runs, this method should be called from a high priority process.
	  => no callbacks should finish while this method runs."

	stackProtect critical: [ callbackInvocationStack push: aCallbackInvocation ].
	aCallbackInvocation callback runStrategy executeCallback: aCallbackInvocation on: self.

]

{ #category : #errors }
TFRunner >> handleExceptionDuring: aBlock [

	self exceptionHandler handleExceptionDuring: aBlock
]

{ #category : #executing }
TFRunner >> invokeFunction: aTFExternalFunction [ 
	
	^ self invokeFunction: aTFExternalFunction withArguments: #()
]

{ #category : #executing }
TFRunner >> invokeFunction: aTFExternalFunction withArguments: aCollection [

	^ (TFPooledExternalAsyncCall forFunction: aTFExternalFunction)
		parameters: aCollection;
		executeOn: self
]

{ #category : #private }
TFRunner >> newSemaphorePool [

	^ TFPool
		newProvider: (MessageSend receiver: TFExternalSemaphore selector: #new) 
		size: self semaphorePoolSize
		releaseBlock: #release "This is ugly... but do not change it to a block, if you change it to a block it will have a reference to self. Creating a loop in the references and leaking memory"
]

{ #category : #private }
TFRunner >> primitivePerformWorkerCall: aTFExternalFunction
		withArguments: argumentHolder
		withReturnHolder: aReturnHolder
		usingSemaphore: anInteger [
		
	^ self subclassResponsibility
]

{ #category : #executing }
TFRunner >> release [ 
	
	semaphorePool ifNotNil: [ 
		semaphorePool release.
		semaphorePool := nil ].
	
	handle beNull.
	"If the stack is not initialized it means this worker was never used for callbacks"
	stackProtect ifNil: [ ^ self ].
	stackProtect critical: [
		callbackInvocationStack := nil ]
]

{ #category : #executing }
TFRunner >> returnCallback: aCallbackInvocation [
	"Entry point to return a callback invocation.
	Check the callbackInvocation stack to see if the given invocation is the last one.
	If so, it can return safely.
	Otherwise, throw an exception as returning means a bug in your application.
	The user must guarantee callbacks return in the correct order"

	callbackReturnSemaphore ifNil: [ 
		callbackReturnSemaphore := Semaphore new ].

	aCallbackInvocation isNull 
		ifTrue: [ ^ self ].

	callbackInvocationStack isEmptyOrNil ifTrue: [ 
		^ self cannotReturnCallbackFromOldSession: aCallbackInvocation].

	stackProtect critical: [
		(callbackInvocationStack top == aCallbackInvocation) 
			ifTrue: [
				aCallbackInvocation returnExecution
					ifTrue: [
						"If we are starting a session and an execution was executed, it 
						 may happen that the stack is empty."
						callbackInvocationStack ifEmpty: [ ^ self ].
						callbackInvocationStack pop.
						[ callbackReturnSemaphore signalAll ] valueUnpreemptively.
						^ self ]] ].
	"failed"
	callbackReturnSemaphore wait.
	self returnCallback: aCallbackInvocation
]

{ #category : #accessing }
TFRunner >> semaphorePool [
	
	self ensureInitialized.
	^ semaphorePool
]

{ #category : #accessing }
TFRunner >> semaphorePoolSize [

	^ 5
]
