"
SUnit tests for stacks
"
Class {
	#name : #AbstractStackTest,
	#superclass : #TestCase,
	#traits : 'TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty})',
	#classTraits : 'TEmptyTest classTrait + TCloneTest classTrait',
	#instVars : [
		'empty',
		'nonEmpty'
	],
	#classInstVars : [
		'testSize'
	],
	#category : #'Collections-Tests-Stacks'
}

{ #category : #testing }
AbstractStackTest class >> isAbstract [
	^ self name = #AbstractStackTest
]

{ #category : #'tests - empty' }
AbstractStackTest >> empty [
	
 	^ empty
]

{ #category : #'tests - empty' }
AbstractStackTest >> nonEmpty [
	
 	^ nonEmpty
]

{ #category : #running }
AbstractStackTest >> setUp [
	super setUp.
	empty := self stackClass new.
	nonEmpty := self stackClass new.
	nonEmpty push: 1.
	nonEmpty push: -2.
	nonEmpty push: 3.
	nonEmpty push: 1.
	
]

{ #category : #accessing }
AbstractStackTest >> stackClass [
	^ self subclassResponsibility
]

{ #category : #tests }
AbstractStackTest >> testEmptyError [
	| aStack |
	aStack := self stackClass new.
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error.
	aStack push: 'element'.
	aStack top.
	aStack pop.	"The stack is empty again due to previous pop"
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error
]

{ #category : #tests }
AbstractStackTest >> testPop [

	| aStack res elem |
	elem := 'anElement'.	
	aStack := self stackClass new.
	self assert: aStack isEmpty.
	
	aStack push: 'a'.
	aStack push: elem.
	res := aStack pop.	
	self assert: res = elem.
	self assert: res == elem.
	
	self assert: aStack size = 1.
	aStack pop.
	self assert: aStack isEmpty.


]

{ #category : #tests }
AbstractStackTest >> testPush [
	
	| aStack |
	aStack := self stackClass new.
	aStack push: 'a'.
	self assert: aStack size = 1.	
	aStack push: 'b'.
	self assert: aStack size = 2.
	
]

{ #category : #tests }
AbstractStackTest >> testSize [
	
	| aStack |
	aStack := self stackClass new.
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
AbstractStackTest >> testTop [

	| aStack |
	aStack := self stackClass new.
	self assert: aStack isEmpty.
	aStack push: 'a'.
	aStack push: 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack size = 2.
]
