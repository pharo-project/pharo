Class {
	#name : 'ReImplementedNotSentRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> calledMethod [

	^ true
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> methodWithReallySpecificNameInTheSystem [

	^ self calledMethod
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> methodWithReallySpecificNameInTheSystemCallingItself [
	false ifTrue: [ self methodWithReallySpecificNameInTheSystemCallingItself ]
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRule [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystem.

	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleDoesNotMatchCalledMethod [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #calledMethod.

	self assertEmpty: critiques
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleOnSelfCall [

	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystemCallingItself.

	self assert: critiques size equals: 1
]
