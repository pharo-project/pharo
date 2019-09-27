"
SUnit tests for stacks
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
	#category : #'Collections-Tests-Stack'
}

{ #category : #'tests - empty' }
StackTest >> empty [
	
 	^ empty
]

{ #category : #'tests - empty' }
StackTest >> nonEmpty [
	
 	^ nonEmpty
]

{ #category : #running }
StackTest >> setUp [
	super setUp.
	empty := NewStack new.
	nonEmpty := NewStack new.
	nonEmpty push: 1.
	nonEmpty push: -2.
	nonEmpty push: 3.
	nonEmpty push: 1.
	
]

{ #category : #tests }
StackTest >> testEmptyError [
	| aStack |
	aStack := NewStack new.
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error.
	aStack push: 'element'.
	aStack top.
	aStack pop.	"The stack is empty again due to previous pop"
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error
]

{ #category : #tests }
StackTest >> testPop [
	| aStack res elem |
	elem := 'anElement'.
	aStack := NewStack new.
	self assertEmpty: aStack.

	aStack push: 'a'.
	aStack push: elem.
	res := aStack pop.
	self assert: res = elem.
	self assert: res == elem.

	self assert: aStack size = 1.
	aStack pop.
	self assertEmpty: aStack
]

{ #category : #tests }
StackTest >> testPush [
	
	| aStack |
	aStack := NewStack new.
	aStack push: 'a'.
	self assert: aStack size = 1.	
	aStack push: 'b'.
	self assert: aStack size = 2.
	
]

{ #category : #tests }
StackTest >> testRemoveAll [
	| aStack |
	aStack := NewStack new.
	aStack push: 1.
	aStack push: 2.
	aStack push: 3.
	
	self assert: aStack size equals: 3.
	
	aStack removeAll.
	
	self assert: aStack isEmpty
]

{ #category : #tests }
StackTest >> testSize [
	
	| aStack |
	aStack := NewStack new.
	self assert: aStack size = 0.
	aStack push: 'a'.
	self assert: aStack size = 1.
	aStack push: 'b'.
	self assert: aStack size = 2.
	aStack pop.
	self assert: aStack size = 1.
	aStack pop.
	self assert: aStack size = 0.

 
	



]

{ #category : #tests }
StackTest >> testTop [
	| aStack |
	aStack := NewStack new.
	self assertEmpty: aStack.
	aStack push: 'a'.
	aStack push: 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack size = 2
]
