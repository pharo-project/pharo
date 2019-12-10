"
A RBMethodSourceContainsLinefeedsRuleTest is a test class for testing the behavior of RBMethodSourceContainsLinefeedsRule
"
Class {
	#name : #RBMethodSourceContainsLinefeedsRuleTest,
	#superclass : #ReAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBMethodSourceContainsLinefeedsRuleTest >> foo [
	"This method contains two linefeeds which should be carriage returns."
	'There is one after this line'.
	'Noone here'.
	'and one here'.
	'That was it.'
]

{ #category : #tests }
RBMethodSourceContainsLinefeedsRuleTest >> testRule [
	| critiques|
 	critiques := self myCritiquesOnFoo .

 	self assert: critiques size equals: 2.
 	self assert: critiques first sourceAnchor interval equals: (109 to: 109).
]
