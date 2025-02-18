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

	self assert: (self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystem) size equals: 1
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleDoesNotMatchCalledMethod [

	self assertEmpty: (self myCritiquesOnMethod: self class >> #calledMethod)
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleOnSelfCall [

	self assert: (self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystemCallingItself) size equals: 1
]
