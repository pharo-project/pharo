Class {
	#name : 'ReTemporaryNeitherReadNorWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRule [

	| critiques |
	self class compile: 'method |unused|' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'method |used| used := 1. ^used'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]
