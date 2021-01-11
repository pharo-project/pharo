"
I'm a threaded ffi worker. 
A worker handles callouts/callbacks in a real thread of the system.

Creating a worker has several consquences both in image and in VM: 

Image side, it creates a process responsible to handle all callback requests from vm.
VM side, it creates a thread and installs a worker there, responsible to collect callouts and callbacks.


"
Class {
	#name : #TFWorker,
	#superclass : #TFRunner,
	#instVars : [
		'name',
		'semaphorePoolHolder'
	],
	#classVars : [
		'Default'
	],
	#category : #'ThreadedFFI-Worker'
}

{ #category : #accessing }
TFWorker class >> default [

	^ Default ifNil: [ Default := self named: 'default' ]
]

{ #category : #private }
TFWorker class >> finalizeResourceData: anArray [

	| handle semaphorePool |
	handle := anArray first.
	semaphorePool := anArray second first.

	(self fromHandle: handle)
		semaphorePool: semaphorePool;
		release.
		
	handle beNull.
]

{ #category : #private }
TFWorker class >> named: aName [

	^ self new 
		name: aName;
		autoRelease;
		yourself
]

{ #category : #accessing }
TFWorker >> doInitialize [

	super doInitialize.
	self primitiveCreateWorker
]

{ #category : #executing }
TFWorker >> executeFunction: aTFExternalFunction withArguments: arguments usingSemaphore: anInteger [ 
	
	<primitive: 'primitiveWorkerCallout' error: ec>
	
	self primitiveFailed 
]

{ #category : #initialization }
TFWorker >> initialize [

	super initialize.
	semaphorePoolHolder := Array new: 1
]

{ #category : #accessing }
TFWorker >> name [

	^ name
]

{ #category : #accessing }
TFWorker >> name: aName [

	name := aName
]

{ #category : #private }
TFWorker >> newSemaphorePool [ 

	"We need to keep a reference to the semaphorePool in an array. Because the array is passed to the finalization registry.
	Like this the semaphorePool can be created lazy."
	semaphorePoolHolder ifNil: [ semaphorePoolHolder := Array new: 1 ].
	semaphorePoolHolder at: 1 put: super newSemaphorePool.
	^ semaphorePoolHolder at: 1
]

{ #category : #private }
TFWorker >> primitiveCreateWorker [

	<primitive: 'primitiveCreateWorker'>

	^ self primitiveFailed 
]

{ #category : #private }
TFWorker >> primitiveReleaseWorker [

	<primitive: 'primitiveReleaseWorker'>

	^ self primitiveFailed 
]

{ #category : #executing }
TFWorker >> readReturnValueFromTask: anExternalAddress [

	<primitive: 'primitiveWorkerExtractReturnValue' error: ec>

	self primitiveFailed
]

{ #category : #executing }
TFWorker >> release [ 
	
	"We need to ensure the semaphorePool is always released"
	semaphorePool ifNotNil: [ 
		semaphorePool release.
		semaphorePoolHolder at: 1 put: nil.
		semaphorePool := nil ].	
	
	self isNull ifTrue: [ ^ self ].
	
	self primitiveReleaseWorker.
	super release.
]

{ #category : #'external resource management' }
TFWorker >> resourceData [

	^ { handle. semaphorePoolHolder }
]

{ #category : #private }
TFWorker >> semaphorePool: aValue [

	semaphorePool := aValue
]
