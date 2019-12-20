"
A RBMultiplePeriodsTerminatingStatementRuleTest is a test class for testing the behavior of RBMultiplePeriodsTerminatingStatementRule
"
Class {
	#name : #RBMultiplePeriodsTerminatingStatementRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBMultiplePeriodsTerminatingStatementRuleTest >> sampleMethod [
	"I have two periods between statements"
	2+3.  .
	{1.2..3}.
	99.
.
]

{ #category : #tests }
RBMultiplePeriodsTerminatingStatementRuleTest >> testRule [
	| critiques |
	critiques := self myCritiques .
	
	self assert: critiques size equals: 2.
	self assert: (self sourceAtChritique: critiques first) equals: '.  .'.
]
