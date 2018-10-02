"
I hold tests for Stack.
"
Class {
	#name : #StackTest,
	#superclass : #TestCase,
	#traits : 'TEmptyTest + TCloneTest',
	#classTraits : 'TEmptyTest classTrait + TCloneTest classTrait',
	#instVars : [
		'empty',
		'nonEmpty'
	],
	#category : #'Collections-Tests-Stacks'
}

{ #category : #helper }
StackTest >> empty [

	^ empty
]

{ #category : #helper }
StackTest >> nonEmpty [

	^ nonEmpty
]

{ #category : #running }
StackTest >> setUp [
	super setUp.
	empty := Stack new.
	nonEmpty := Stack new.
	nonEmpty push: 1.
	nonEmpty push: -2.
	nonEmpty push: 3.
	nonEmpty push: 1.
	
]
