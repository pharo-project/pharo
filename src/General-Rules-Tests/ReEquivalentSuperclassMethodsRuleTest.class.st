Class {
	#name : 'ReEquivalentSuperclassMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRule [

	| critiques |
	self class compile: 'myCritiques ^ self myCritiquesOnMethod: self class >> #sampleMethod'.
	critiques := self myCritiquesOnMethod: self class >> #myCritiques.
	self assert: critiques size equals: 1.
	[ "code of the test" ] ensure: [ (self class >> #myCritiques ) removeFromSystem ]
]

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'myCritiques ^true'.
	critiques := self myCritiquesOnMethod: self class >> #myCritiques.
	self assertEmpty: critiques.
	[ "code of the test" ] ensure: [ (self class >> #myCritiques ) removeFromSystem ]
]
