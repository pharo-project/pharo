"
A RBSentNotImplementedRuleTest is a test class for testing the behavior of RBSentNotImplementedRule
"
Class {
	#name : #RBSentNotImplementedRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBSentNotImplementedRuleTest >> foo [
	"this is a comment, so whatShallIDoNow should not highlight"
	99 whatShallIDoNow.
	77 andThisIsBogusToo.
	88 even.
	88 whatShallIDoNow
]

{ #category : #tests }
RBSentNotImplementedRuleTest >> testRule [
	| critiques |
	critiques := self myCritiquesOnFoo.
	self assert: critiques size equals: 3.
	self assert: critiques first sourceAnchor interval equals: (68 to: 85)
]
