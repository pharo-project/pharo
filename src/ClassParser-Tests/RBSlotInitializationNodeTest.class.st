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
	s1 := RBSlotInitializationNode selectorParts: #(#default:) argumentParts: 5.
	s2 := RBSlotInitializationNode selectorParts: #(#default:) argumentParts: 5.
	self assert: s1 equals: s2
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testIsNull [
	| s |
	s := RBSlotInitializationNode new.
	self assert: s selectorString equals: ''.
	self assert: s isNull
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testPrintString [
	| s1 |
	s1 := RBSlotInitializationNode selectorParts: #(#default:) argumentParts: #(5).
	self assert: s1 printString equals: 'default: 5'
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testPrintStringANullInitializationMessage [
	| s1 |
	s1 := RBSlotInitializationNode new.
	self assert: s1 printString equals: ''
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testPrintStringMultiple [
	| s1 |
	s1 := RBSlotInitializationNode selectorParts: #(default: named:) argumentParts: #(5 'titi').
	self assert: s1 printString equals: 'default: 5 named: ''titi'''
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testSelector [
	| s1 |
	s1 := RBSlotInitializationNode selectorParts: #default:named: argumentParts: #(5 'titi').
	self assert: s1 selectorString equals: 'default:named:'.
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testSlotInitializationCreation [

	| s | 
	s := RBSlotInitializationNode selectorParts: #(#default:) argumentParts: #(5).
	self assert: s selectorString equals: #default:.
	self assert: s argumentParts equals: #(5)
]

{ #category : #tests }
RBSlotInitializationNodeTest >> testSlotMultipleInitializationCreation [

	| s | 
	s := RBSlotInitializationNode selectorParts: #(default: named:) argumentParts: #(5 'titi').
	self assert: s selectorParts equals: #(default: named:).
	self assert: s selectorString equals: 'default:named:'.
	self assert: s argumentParts equals: #(5 'titi')
]
