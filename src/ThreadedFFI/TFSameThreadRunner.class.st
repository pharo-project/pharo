"
I run the callouts in the same thread of the VM.
This runner blocks the current execution of the VM to run the callout.
"
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
	handle := self primitiveGetRunnerAddress
]

{ #category : #executing }
TFSameThreadRunner >> invokeFunction: function withArguments: aCollection [

	function validate.
	^ self primitivePerformWorkerCall: function withArguments: aCollection
]

{ #category : #private }
TFSameThreadRunner >> primitiveGetRunnerAddress [

	<primitive: 'primitiveGetSameThreadRunnerAddress'>

	self primitiveFailed
]

{ #category : #private }
TFSameThreadRunner >> primitivePerformWorkerCall: aTFExternalFunction withArguments: argumentHolder [

	<primitive: 'primitiveSameThreadCallout' error: ec>

	self primitiveFailed
]
