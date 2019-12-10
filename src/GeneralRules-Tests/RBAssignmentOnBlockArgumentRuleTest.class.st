"
A RBAssignmentOnBlockArgumentRuleTest is a test class for testing the behavior of RBAssignmentOnBlockArgumentRule
"
Class {
	#name : #RBAssignmentOnBlockArgumentRuleTest,
	#superclass : #RBAbstractRuleTestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'test-help' }
RBAssignmentOnBlockArgumentRuleTest >> foo [
	"Assignment to blockArgument is bad - here x"
	| myBlock |
	myBlock := [:x :y|
		x := x+y.
		y := y+x
	]
]

{ #category : #tests }
RBAssignmentOnBlockArgumentRuleTest >> testRule [
	| critiques|
 	critiques := self myCritiquesOnFoo.
	
 	self assert: critiques size equals: 2.
 	self assert: critiques second sourceAnchor interval equals: (99 to: 106).
]
