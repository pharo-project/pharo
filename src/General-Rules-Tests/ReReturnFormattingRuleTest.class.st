Class {
	#name : 'ReReturnFormattingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReReturnFormattingRuleTest >> testRule [

	| critiques |
	
	self class compile: 'invalid #(true false) atRandom ifTrue: [ ^1 ] ifFalse: [ ^  2 ]' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #invalid.
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #invalid) removeFromSystem ]
]

{ #category : 'tests' }
ReReturnFormattingRuleTest >> testRuleNotViolated [

	| critiques |
	
	self class compile: 'valid ^ 1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #valid.
	self assertEmpty: critiques ] ensure: [ (self class >> #valid) removeFromSystem ]
]
