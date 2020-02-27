"
I am an example for a class variable that stores the value in the variable. It behaves thus the same as a normal class variable, but the state is not stored in the class pool but in the variable itself.

Read and write is done via #read and #write:.
"
Class {
	#name : #ExampleClassVariableWithState,
	#superclass : #LiteralVariable,
	#instVars : [
		'state'
	],
	#category : #'Slot-Examples-ClassVariables'
}

{ #category : #'meta-object-protocol' }
ExampleClassVariableWithState >> read [
	^state
]

{ #category : #'meta-object-protocol' }
ExampleClassVariableWithState >> write:	 anObject [
	state := anObject
	 
]
