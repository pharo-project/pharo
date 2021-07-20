"
A RBMultiplePeriodsTerminatingStatementRuleTest is a test class for testing the behavior of RBMultiplePeriodsTerminatingStatementRule
"
Class {
	#name : #ReMultiplePeriodsTerminatingStatementRuleTest,
	#superclass : #ReAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #sample }
ReMultiplePeriodsTerminatingStatementRuleTest >> sampleMethod [
	"I have two periods between statements"
	2+3.  .
	{1.2..3}.
	99.
.
]

{ #category : #tests }
ReMultiplePeriodsTerminatingStatementRuleTest >> testRule [
	| critiques |
	critiques := self myCritiques .
	
	self assert: critiques size equals: 2.
	self assert: (self sourceAtChritique: critiques first) equals: '.  .'.
]
