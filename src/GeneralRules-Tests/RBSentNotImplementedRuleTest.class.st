"
A RBSentNotImplementedRuleTest is a test class for testing the behavior of RBSentNotImplementedRule
"
Class {
	#name : #RBSentNotImplementedRuleTest,
	#superclass : #TestCase,
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
	| method critiques |
	method := self class >> #foo.
	critiques := OrderedCollection new.
	RBSentNotImplementedRule new 
		check: method
		forCritiquesDo: [ :critique | critiques add: critique ].
	self assert: critiques size equals: 3.
	self assert: critiques first sourceAnchor interval equals: (71 to: 85)
]
