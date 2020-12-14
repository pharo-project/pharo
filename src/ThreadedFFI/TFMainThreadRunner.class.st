Class {
	#name : #TFMainThreadRunner,
	#superclass : #TFRunner,
	#classVars : [
		'MainThreadHandle'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'ThreadedFFI-Worker'
}

{ #category : #accessing }
TFMainThreadRunner class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #private }
TFMainThreadRunner >> doInitialize [
	
	super doInitialize.
	MainThreadHandle := self workerAddress.
	handle := MainThreadHandle
]

{ #category : #executing }
TFMainThreadRunner >> executeFunction: aTFExternalFunction withArguments: arguments usingSemaphore: anInteger [ 
	<primitive: 'primitiveWorkerCallout' error: ec>
	
	self primitiveFailed 
]

{ #category : #private }
TFMainThreadRunner >> primitivePerformWorkerCall: aTFExternalFunction withArguments: argumentHolder withReturnHolder: aReturnHolder usingSemaphore: anInteger [ 
	
	<primitive: 'primitivePerformWorkerCall'>

	^ self primitiveFailed 
]

{ #category : #executing }
TFMainThreadRunner >> readReturnValueFromTask: anExternalAddress [
	<primitive: 'primitiveWorkerExtractReturnValue' error: ec>

	self primitiveFailed
]

{ #category : #private }
TFMainThreadRunner >> workerAddress [

	^ (ExternalAddress loadSymbol: 'mainThreadWorker') pointerAt: 1
]
