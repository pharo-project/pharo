"
I implement a Callout for the TFSameThreadRunner
"
Class {
	#name : #TFSameThreadCall,
	#superclass : #Object,
	#instVars : [
		'function'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFSameThreadCall class >> forFunction: aTFExternalFunction [ 
	
	^ self new
		function: aTFExternalFunction;
		yourself
]

{ #category : #executing }
TFSameThreadCall >> executeOn: runner withArguments: aCollection [

	function validate.	
	^ self invokeFunction: function withArguments: aCollection
	
]

{ #category : #accessing }
TFSameThreadCall >> function [
	^ function
]

{ #category : #accessing }
TFSameThreadCall >> function: anObject [
	function := anObject
]

{ #category : #toto }
TFSameThreadCall >> invokeFunction: aTFExternalFunction withArguments: aCollection [
	
	<primitive: 'primitiveSameThreadCallout' error: ec>

	self primitiveFailed 
]

{ #category : #accessing }
TFSameThreadCall >> parameters: aCollection [ 
	self shouldBeImplemented.
]
