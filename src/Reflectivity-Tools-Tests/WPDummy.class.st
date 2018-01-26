"
I am a dummy class to test watchpoints
"
Class {
	#name : #WPDummy,
	#superclass : #Object,
	#instVars : [
		'foo'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #examples }
WPDummy >> exampleAssignment [
	foo := 1.
	
	
]

{ #category : #examples }
WPDummy >> exampleAssignment: anObject [
	foo := anObject.
	
	
]

{ #category : #examples }
WPDummy >> exampleMessageSend [
	self exampleOperation
	
	
]

{ #category : #examples }
WPDummy >> exampleOperation [
	<sampleInstance>
	^ 2+3
]

{ #category : #examples }
WPDummy >> exampleOperationAndAssignment [
	|a|
	a := 1 + 1.
	foo := a + a.
	
	
]

{ #category : #examples }
WPDummy >> exampleOperationAndAssignment: aNumber [
	|a|
	a := aNumber.
	foo := a + a.
	
	
]

{ #category : #accessing }
WPDummy >> foo [
	^ foo
]

{ #category : #accessing }
WPDummy >> foo: anObject [
	foo := anObject
]
