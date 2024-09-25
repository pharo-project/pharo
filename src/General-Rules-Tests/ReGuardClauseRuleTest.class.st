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

	self class compile: 'method | arg | arg := 1. arg > 2 ifTrue: [ 1. 1. ] ' classified: 'test-helper'.
	[
	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'method | arg | arg := 1. arg > 2 ifFalse: [ ^ self ]. 1. 1. ' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReGuardClauseRuleTest >> testRuleNotViolatedRegressionTest [
	"This is a regression test. The method should not produce any violation but at some point it produced it."

	self class
		compile: 'method

	| boolean |
	boolean := true.
	boolean ifTrue: [
		boolean := false.
		self hash ].
	self hash'
		classified: 'test-helper'.
	[
	| critiques |
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
