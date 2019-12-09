"
A RBMultiplePeriodsTerminatingStatementRuleTest is a test class for testing the behavior of RBMultiplePeriodsTerminatingStatementRule
"
Class {
	#name : #RBMultiplePeriodsTerminatingStatementRuleTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBMultiplePeriodsTerminatingStatementRuleTest >> foo [
	"I have two periods between statements"
	2+3.  .
	{1.2..3}.
	99.
.
]

{ #category : #tests }
RBMultiplePeriodsTerminatingStatementRuleTest >> testRule [
	| method critiques |
	method := self class >> #foo.
	critiques := OrderedCollection new.
	RBMultiplePeriodsTerminatingStatementRule new 
		check: method
		forCritiquesDo: [ :critique | critiques add: critique ].
	
	self assert: critiques size equals: 2.
	self assert: critiques second sourceAnchor interval equals: (69 to: 71)
]
