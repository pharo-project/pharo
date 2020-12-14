Class {
	#name : #TFSameThreadRunner,
	#superclass : #TFRunner,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'ThreadedFFI-Worker'
}

{ #category : #accessing }
TFSameThreadRunner class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #executing }
TFSameThreadRunner >> doInitialize [

	super doInitialize.
	handle := self primitiveGetRunnerAddress.
]

{ #category : #executing }
TFSameThreadRunner >> invokeFunction: aTFExternalFunction withArguments: aCollection [ 

	^ (	TFSameThreadCall forFunction: aTFExternalFunction) executeOn: self withArguments: aCollection
]

{ #category : #private }
TFSameThreadRunner >> primitiveGetRunnerAddress [

	<primitive: 'primitiveGetSameThreadRunnerAddress'>

	self primitiveFailed 	
]

{ #category : #private }
TFSameThreadRunner >> primitivePerformWorkerCall: aTFExternalFunction withArguments: argumentHolder withReturnHolder: aReturnHolder usingSemaphore: anInteger [ 

	<primitive: 'primitiveSameThreadCallout'>

	self primitiveFailed 	
]
