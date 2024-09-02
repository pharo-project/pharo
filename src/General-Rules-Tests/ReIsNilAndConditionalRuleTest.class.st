Class {
	#name : 'ReIsNilAndConditionalRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleNotViolatedWithIfNil [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifNil: [ arg := 1 ]' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleNotViolatedWithIfNilIfNotNil [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifNil: [ arg := 1 ] ifNotNil: [ arg := 2 ] ' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleNotViolatedWithIfNotNil [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifNotNil: [ arg := 2 ] ' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleWithIsNilIfFalse [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifFalse: [ arg := 1 ] ' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleWithIsNilIfTrue [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifTrue: [ arg := 1 ] ' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNilAndConditionalRuleTest >> testRuleWithIsNilIfTrueIfFalse [

	| critiques |
	self class compile: 'method | arg | ^ arg isNil ifTrue: [ arg := 1 ] ifFalse: [ arg := 2 ] ' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
