"
A RBAssignmentOnBlockArgumentRuleTest is a test class for testing the behavior of RBAssignmentOnBlockArgumentRule
"
Class {
	#name : #RBAssignmentOnBlockArgumentRuleTest,
	#superclass : #TestCase,
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
	|method critiques|
 	method := self class >> #foo.
 	critiques := OrderedCollection new.
 	RBAssignmentOnBlockArgumentRule new 
		check: method forCritiquesDo:[:critique | critiques add: critique].

 	self assert: critiques size equals: 2.
 	self assert: critiques second sourceAnchor interval equals: (99 to: 106).
]
