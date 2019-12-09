"
A RBSelfSentNotImplementedRuleTest is a test class for testing the behavior of RBSelfSentNotImplementedRule
"
Class {
	#name : #RBSelfSentNotImplementedRuleTest,
	#superclass : #TestCase,
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #'as yet unclassified' }
RBSelfSentNotImplementedRuleTest >> foo [
	"Do not highlight this bar. I am a method used in testCheckForCritiques"

	self bar.
	self bar; bar.
	(2 + 5) bar.
	7 < 10 ifTrue: [ self check:7 forCritiquesDo: 9 ]
]

{ #category : #tests }
RBSelfSentNotImplementedRuleTest >> testCheckForCritiques [
	|method critiques|
	method := self class >> #foo.
	critiques := OrderedCollection new.
	RBSelfSentNotImplementedRule new check: method forCritiquesDo:[:critique | critiques add: critique].
	
	self assert: critiques size equals: 4.
	self assert: critiques first sourceAnchor interval equals: (86 to: 88).
	
]
