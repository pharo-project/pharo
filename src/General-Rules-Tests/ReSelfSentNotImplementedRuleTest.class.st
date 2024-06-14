"
A RBSelfSentNotImplementedRuleTest is a test class for testing the behavior of RBSelfSentNotImplementedRule
"
Class {
	#name : 'ReSelfSentNotImplementedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'sample' }
ReSelfSentNotImplementedRuleTest >> sampleMethod [
	"Do not highlight this bar. I am a method used in testCheckForCritiques"

	self bar.
	self bar; bar.
	(2 + 5) bar.
	7 < 10 ifTrue: [ self check:7 forCritiquesDo: 9 ]
]

{ #category : 'tests' }
ReSelfSentNotImplementedRuleTest >> testRule [
	| critiques|
	critiques := self myCritiques .

	self assert: critiques size equals: 4.
	self assert: (self sourceAtChritique: critiques first) equals: 'self bar'
]
