Class {
	#name : 'ReDetectIfNoneRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReDetectIfNoneRuleTest >> testRuleNotViolated [ 

	| critiques |
	self class
		compile:
		'method | temp | temp := OrderedCollection new. temp anySatisfy: [ :each | each > 1 ]'
		classified: 'test-helper'.
		[ critiques := self myCritiquesOnMethod: self class >> #method.
		self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDetectIfNoneRuleTest >> testRuleWithContains [

	| critiques |
	self class
		compile:
		'method | temp | temp := OrderedCollection new. temp contains: [ :each | each > 1 ] '
		classified: 'test-helper'.
		[ critiques := self myCritiquesOnMethod: self class >> #method.
		self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDetectIfNoneRuleTest >> testRuleWithDetect [

	| critiques |
	self class
		compile:
		'method | temp | temp := OrderedCollection new. (temp detect: [ :each | each > 1 ] ifNone: [ nil ] ) isNil'
		classified: 'test-helper'.
		[ critiques := self myCritiquesOnMethod: self class >> #method.
		self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
