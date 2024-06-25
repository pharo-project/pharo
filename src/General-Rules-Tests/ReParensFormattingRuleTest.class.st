Class {
	#name : 'ReParensFormattingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReParensFormattingRuleTest >> testRule [

	| critiques |
	self class
		compile: 'invalid

	( 1 + 2).
	(1 + 2 )'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #invalid.
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #invalid) removeFromSystem ]
]

{ #category : 'tests' }
ReParensFormattingRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'valid

	(1 + 2)'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #valid.
	self assertEmpty: critiques ] ensure: [ (self class >> #valid) removeFromSystem ]
]
