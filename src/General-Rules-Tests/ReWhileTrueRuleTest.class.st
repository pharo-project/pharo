Class {
	#name : 'ReWhileTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReWhileTrueRuleTest >> testRule [

	| critiques |
	self class
		compile: 'method 	| index stop |
	stop := 5.
	index := 1.
	[ index <= stop ]
		whileTrue: [
			index := index + 1 ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method)  removeFromSystem]
]

{ #category : 'tests' }
ReWhileTrueRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'method 	| index stop |
	stop := 5.
	index := 1.
	index to: stop do: [
			index := index + 1 ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method)  removeFromSystem]
]
