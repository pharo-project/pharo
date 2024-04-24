Class {
	#name : 'ReUseSetUpRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRule [

	| critiques |
	self class compile: 'initialize' classified: 'initialization'.
	critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assert: critiques size equals: 1.
	[ "code of the test" ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'setUp'.
	critiques := self myCritiquesOnClass: self class.
	self assertEmpty: critiques.
	[ "code of the test" ] ensure: [
		(self class >> #setUp) removeFromSystem ]
]
