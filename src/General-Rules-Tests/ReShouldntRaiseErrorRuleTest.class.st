Class {
	#name : 'ReShouldntRaiseErrorRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReShouldntRaiseErrorRuleTest >> testRule [

	| critiques |
	self class compile: 'method self shouldnt: [ 1. ] raise: Error' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1  ] ensure: [ (self class >> #method) removeFromSystem ]
]
