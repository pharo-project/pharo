"
A RBSentNotImplementedRuleTest is a test class for testing the behavior of RBSentNotImplementedRule
"
Class {
	#name : 'ReSentNotImplementedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Bugs'
}

{ #category : 'utilities' }
ReSentNotImplementedRuleTest >> myCritiquesOnMethod: method [

	| methodCopy newProperties |
	methodCopy := method copy.
	newProperties := methodCopy properties copyWithout: methodCopy pragmas first.
	methodCopy properties: newProperties.

	^ super myCritiquesOnMethod: methodCopy
]

{ #category : 'sample' }
ReSentNotImplementedRuleTest >> sampleMethod [

	<ignoreNotImplementedSelectors: #( whatShallIDoNow andThisIsBogusToo )>
	"this is a comment, so whatShallIDoNow should not highlight"
	99 whatShallIDoNow.
	77 andThisIsBogusToo.
	88 even.
	88 whatShallIDoNow
]

{ #category : 'tests' }
ReSentNotImplementedRuleTest >> testRule [
	| critiques |
	critiques := self myCritiques.

	self assert: critiques size equals: 3.
	self assert: (self sourceAtChritique:  critiques third) equals: '88 whatShallIDoNow'
]
