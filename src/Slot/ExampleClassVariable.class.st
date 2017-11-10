"
I am an example for a class variable that stores the value in the variable.
(same semantics as normal variables but using reflective code path for testing)
"
Class {
	#name : #ExampleClassVariable,
	#superclass : #LiteralVariable,
	#instVars : [
		'state'
	],
	#category : #Slot-Examples
}

{ #category : #meta-object-protocol }
ExampleClassVariable >> read [
	^state
]

{ #category : #meta-object-protocol }
ExampleClassVariable >> write:	 anObject [
	state := anObject
	 
]
