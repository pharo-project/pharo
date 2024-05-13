Class {
	#name : 'ReYourselfNotUsedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
ReYourselfNotUsedRuleTest >> testRule [

	| critiques |
	self class
		compile: 'methodTest self class yourself'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #methodTest.
	self assert: critiques size equals: 1.
	 ] ensure: [
		(self class >> #methodTest) removeFromSystem ]
]

{ #category : 'tests' }
ReYourselfNotUsedRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'methodTest ^ self yourself'
		classified: 'test-helper'.
	[critiques := self myCritiquesOnMethod: self class >> #methodTest.
	self assertEmpty: critiques.
	] ensure: [
		(self class >> #methodTest) removeFromSystem ]
]
