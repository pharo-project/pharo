"
A ReSentButNotUnderstoodBySuperRuleTest is a test class for testing the behavior of ReSentButNotUnderstoodBySuperRule
"
Class {
	#name : 'ReSentButNotUnderstoodBySuperRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReSentButNotUnderstoodBySuperRuleTest >> sampleMethod [
	"This method sends a message the superclass does not understand"
	
	self asString.
	^ super someMessageThatSuperDoesNotUnderstand
.
]

{ #category : 'tests' }
ReSentButNotUnderstoodBySuperRuleTest >> testRule [
	| critiques|
	critiques := self myCritiques .

	self assert: critiques size equals: 1.
	self assert: (self sourceAtChritique: critiques first) equals: 'super someMessageThatSuperDoesNotUnderstand'
]
