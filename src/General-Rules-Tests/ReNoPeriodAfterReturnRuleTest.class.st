Class {
	#name : 'ReNoPeriodAfterReturnRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReNoPeriodAfterReturnRuleTest >> testRule [

	| critiques |
	self class
		compile: 'periodAfterReturn

	#(true false) atRandom ifTrue: [
		self crTrace: ''1''.
		^ self crTrace: ''2''. ].
	   self crTrace: ''3''.
	   ^ self crTrace: ''4''.'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #periodAfterReturn.
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #periodAfterReturn) removeFromSystem ]
]

{ #category : 'tests' }
ReNoPeriodAfterReturnRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'lastPeriods

	#(true false) atRandom ifTrue: [
		self crTrace: ''1''.
		self crTrace: ''2''. ].
		self crTrace: ''3''.
		self crTrace: ''4''.'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #lastPeriods.
	self assertEmpty: critiques ] ensure: [ (self class >> #lastPeriods) removeFromSystem ]
]
