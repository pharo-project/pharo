"
A ReGuardClauseRuleTest is a test class for testing the behavior of ReGuardClauseRule
"
Class {
	#name : 'ReGuardClauseRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRule [

	self generateMethod: 'method | arg | arg := 1. arg > 2 ifTrue: [ 1. 1. ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRuleNotViolated [

	self generateMethod: 'method | arg | arg := 1. arg > 2 ifFalse: [ ^ self ]. 1. 1. '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRuleNotViolatedRegressionTest [
	"This is a regression test. The method should not produce any violation but at some point it produced it."

	self generateMethod: 'method

	| boolean |
	boolean := true.
	boolean ifTrue: [
		boolean := false.
		self hash ].
	self hash'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
