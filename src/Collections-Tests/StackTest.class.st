"
SUnit tests for stacks
"
Class {
	#name : #StackTest,
	#superclass : #TestCase,
	#traits : 'TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty})',
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
	empty := Stack new.
	nonEmpty := Stack new.
	nonEmpty push: 1.
	nonEmpty push: -2.
	nonEmpty push: 3.
	nonEmpty push: 1.
	
]

{ #category : #tests }
StackTest >> testEmptyError [
	| aStack |
	aStack := Stack new.
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
	aStack := Stack new.
	self assertEmpty: aStack.

	aStack push: 'a'.
	aStack push: elem.
	res := aStack pop.
	self assert: res equals: elem.
	self assert: res identicalTo: elem.

	self assert: aStack size equals: 1.
	aStack pop.
	self assertEmpty: aStack
]

{ #category : #tests }
StackTest >> testPush [
	| aStack |
	aStack := Stack new.
	aStack push: 'a'.
	self assert: aStack size equals: 1.
	aStack push: 'b'.
	self assert: aStack size equals: 2
]

{ #category : #tests }
StackTest >> testSize [
	| aStack |
	aStack := Stack new.
	self assert: aStack size equals: 0.
	aStack push: 'a'.
	self assert: aStack size equals: 1.
	aStack push: 'b'.
	self assert: aStack size equals: 2.
	aStack pop.
	self assert: aStack size equals: 1.
	aStack pop.
	self assert: aStack size equals: 0
]

{ #category : #tests }
StackTest >> testTop [
	| aStack |
	aStack := Stack new.
	self assertEmpty: aStack.
	aStack push: 'a'.
	aStack push: 'b'.
	self assert: aStack top equals: 'b'.
	self assert: aStack top equals: 'b'.
	self assert: aStack size equals: 2
]
