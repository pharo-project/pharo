Class {
	#name : 'ReBetweenAndRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReBetweenAndRuleTest >> testRule [

	| critiques |
	self class compile: 'a:a b:b c:c a >= b and: [a <= c]'.
	[ critiques := self myCritiquesOnMethod: self class >> #a:b:c:.
	self assert: critiques size equals: 1
	] ensure: [
		(self class >> #a:b:c:) removeFromSystem ]
]

{ #category : 'tests' }
ReBetweenAndRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'a:a b:b c:c a between: b and: c'.
	[
	critiques := self myCritiquesOnMethod: self class >> #a:b:c:.
	self assertEmpty: critiques ] ensure: [
		(self class >> #a:b:c:) removeFromSystem ]
]
