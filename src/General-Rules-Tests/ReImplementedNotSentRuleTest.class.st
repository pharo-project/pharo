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
ReImplementedNotSentRuleTest >> first [
	"Sends its own selector to another object, while that selector is also sent in many other places in the system"

	^ compiledMethods first
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> methodWithReallySpecificNameInTheSystem [

	^ self calledMethod
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> methodWithReallySpecificNameInTheSystemCallingItself [
	false ifTrue: [ self methodWithReallySpecificNameInTheSystemCallingItself ]
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes [

	false ifTrue: [
		self methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes.
		self methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes ].
	#(  ) do: [ :each | self methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes ]
]

{ #category : 'test-help' }
ReImplementedNotSentRuleTest >> otherSenderOfMethodCallingItselfThreeTimes [

	self methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes
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
ReImplementedNotSentRuleTest >> testRuleDoesNotMatchMethodCallingItselfSeveralTimesAndCalledOnceElsewhere [

	self assertEmpty: (self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystemCallingItselfThreeTimes)
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleDoesNotMatchMethodSendingItsSelectorToAnotherObject [

	self assertEmpty: (self myCritiquesOnMethod: self class >> #first)
]

{ #category : 'tests' }
ReImplementedNotSentRuleTest >> testRuleOnSelfCall [

	self assert: (self myCritiquesOnMethod: self class >> #methodWithReallySpecificNameInTheSystemCallingItself) size equals: 1
]
