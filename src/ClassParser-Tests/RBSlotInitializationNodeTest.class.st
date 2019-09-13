"
A RBSlotInitializationNodeTest is a test class for testing the behavior of RBSlotInitializationNode
"
Class {
	#name : #RBSlotInitializationNodeTest,
	#superclass : #TestCase,
	#category : #'ClassParser-Tests'
}

{ #category : #tests }
RBSlotInitializationNodeTest >> testEquality [
	| s1 s2 |
	s1 := RBSlotInitializationNode selector: #default: argument: 5.

	s2 := RBSlotInitializationNode selector: #default: argument: 5.

	self assert: s1 = s2
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testIsNull [
	| s |
	s := RBSlotInitializationNode new.
	self assert: s selector equals: #''.
	self assert: s isNull
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testPrintString [
	| s1 |
	s1 := RBSlotInitializationNode selector: #default: argument: 5.
	self assert: s1 printString equals: 'default: 5'
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testPrintStringANullInitializationMessage [
	| s1 |
	s1 := RBSlotInitializationNode new.
	self assert: s1 printString equals: ''
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testSlotInitializationCreation [

	| s | 
	s := RBSlotInitializationNode selector: #default: argument: 5.
	self assert: s selector equals: #default:.
	self assert: s argument equals: 5
]
