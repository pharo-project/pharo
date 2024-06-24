Class {
	#name : 'ReGuardClauseRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRule [

	| critiques |
	self class compile: 'method | arg | arg := 1. arg > 2 ifTrue: [ 1. 1. ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method)  removeFromSystem ]
]

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRuleNotViolated [ 

	| critiques |
	self class compile: 'method | arg | arg := 1. arg > 2 ifFalse: [ ^ self ]. 1. 1. ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method)  removeFromSystem ]
]
