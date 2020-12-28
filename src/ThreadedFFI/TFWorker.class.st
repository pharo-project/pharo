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
		'name'
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
TFWorker class >> finalizeResourceData: handle [

	handle isNull ifTrue: [ ^ self ].
	
	(self fromHandle: handle)
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

{ #category : #accessing }
TFWorker >> name [

	^ name
]

{ #category : #accessing }
TFWorker >> name: aName [

	name := aName
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

{ #category : #accessing }
TFWorker >> release [ 
	
	self isNull ifTrue: [ ^ self ].
	
	self primitiveReleaseWorker.
	super release.
]
