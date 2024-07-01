Class {
	#name : 'ReIfTrueIfFalseUselessRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReIfTrueIfFalseUselessRuleTest >> testRule [

	| critiques |
	self class
		compile:
		'method 1 = 1 ifTrue: [ ^ true ] ifFalse: [ ^ false ] '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIfTrueIfFalseUselessRuleTest >> testRuleNotViolated [ 

	| critiques |
	self class
		compile:
		'method ^ ((1 = 1) & (2 = 2)) '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]
