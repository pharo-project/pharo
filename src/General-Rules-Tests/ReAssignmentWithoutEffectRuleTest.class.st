Class {
	#name : 'ReAssignmentWithoutEffectRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAssignmentWithoutEffectRuleTest >> testRule [

	| critiques |
	self class compile: 'method |x| x:=x' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAssignmentWithoutEffectRuleTest >> testRuleNotViolated [

	| critiques |
	self class compile: 'method |x| x:=1' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]
