Class {
	#name : 'ReCollectionAtCollectionSizeRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReCollectionAtCollectionSizeRuleTest >> testRule [

	| critiques |
	self class
		compile:
		'method  | col | col := OrderedCollection. col add: 1; add: 2; add: 3. ^ col at: col size'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCollectionAtCollectionSizeRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile:
		'method  | col | col := OrderedCollection. col add: 1; add: 2; add: 3. ^ col last '
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
