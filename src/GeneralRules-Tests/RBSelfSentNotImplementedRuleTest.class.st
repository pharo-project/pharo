"
A RBSelfSentNotImplementedRuleTest is a test class for testing the behavior of RBSelfSentNotImplementedRule
"
Class {
	#name : #RBSelfSentNotImplementedRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBSelfSentNotImplementedRuleTest >> foo [
	"Do not highlight this bar. I am a method used in testCheckForCritiques"

	self bar.
	self bar; bar.
	(2 + 5) bar.
	7 < 10 ifTrue: [ self check:7 forCritiquesDo: 9 ]
]

{ #category : #tests }
RBSelfSentNotImplementedRuleTest >> testCheckForCritiques [
	| critiques|
	critiques := self myCritiquesOnFoo .
	
	self assert: critiques size equals: 4.
	self assert: critiques first sourceAnchor interval equals: (81 to: 88).
	
]
