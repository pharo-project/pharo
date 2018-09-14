"
SUnit tests for stacks
"
Class {
	#name : #AbstractStackStrategyTest,
	#superclass : #TestCase,
	#instVars : [
		'strategy'
	],
	#classInstVars : [
		'testSize'
	],
	#category : #'Collections-Tests-Stacks'
}

{ #category : #testing }
AbstractStackStrategyTest class >> isAbstract [
	^ self name = #AbstractStackStrategyTest
]

{ #category : #running }
AbstractStackStrategyTest >> setUp [
	super setUp.
	strategy := self stackStrategyClass new
	
]

{ #category : #accessing }
AbstractStackStrategyTest >> stackStrategyClass [
	^ self subclassResponsibility
]

{ #category : #tests }
AbstractStackStrategyTest >> testEmptyError [
	| aStack |
	aStack := strategy.
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error.
	aStack push: 'element'.
	aStack top.
	aStack pop.	"The stack is empty again due to previous pop"
	self should: [ aStack top ] raise: Error.
	self should: [ aStack pop ] raise: Error
]

{ #category : #tests }
AbstractStackStrategyTest >> testPop [

	| res elem aStack |
	aStack := strategy.
	elem := 'anElement'.	
	self assert: aStack size equals: 0.
	
	aStack push: 'a'.
	aStack push: elem.
	res := aStack pop.	
	self assert: res equals: elem.
	self assert: res == elem.
	
	self assert: aStack size equals: 1.
	aStack pop.
	self assert: aStack size equals: 0.


]

{ #category : #tests }
AbstractStackStrategyTest >> testPush [
	| aStack |
	aStack := strategy.
	aStack push: 'a'.
	self assert: aStack size equals: 1.	
	aStack push: 'b'.
	self assert: aStack size equals: 2.
	
]

{ #category : #tests }
AbstractStackStrategyTest >> testSize [
	
	| aStack |
	aStack := strategy.
	self assert: aStack size equals: 0.
	aStack push: 'a'.
	self assert: aStack size equals: 1.
	aStack push: 'b'.
	self assert: aStack size equals: 2.
	aStack pop.
	self assert: aStack size equals: 1.
	aStack pop.
	self assert: aStack size equals: 0.

 
	



]

{ #category : #tests }
AbstractStackStrategyTest >> testTop [

	| aStack |
	aStack := strategy.
	self assert: aStack size equals: 0.
	aStack push: 'a'.
	aStack push: 'b'.
	self assert: aStack top equals: 'b'.
	self assert: aStack top equals: 'b'.
	self assert: aStack size equals: 2.
]
