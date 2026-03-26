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

{ #category : 'utilities' }
ReSentButNotUnderstoodBySuperRuleTest >> myCritiquesOnMethod: method [

	| methodCopy newProperties |
	methodCopy := method copy.
	newProperties := methodCopy properties copyWithout: methodCopy pragmas first.
	methodCopy properties: newProperties.

	^ super myCritiquesOnMethod: methodCopy
]

{ #category : 'accessing' }
ReSentButNotUnderstoodBySuperRuleTest >> sampleMethod [
	"This method sends a message the superclass does not understand"
	<ignoreNotImplementedSelectors: #( someMessageThatSuperDoesNotUnderstand )>
	
	self asString.
	^ super someMessageThatSuperDoesNotUnderstand
]

{ #category : 'tests' }
ReSentButNotUnderstoodBySuperRuleTest >> testRule [
	| critiques|
	critiques := self myCritiques .

	self assert: critiques size equals: 1.
	self assert: (self sourceAtChritique: critiques first) equals: 'super someMessageThatSuperDoesNotUnderstand'
]
