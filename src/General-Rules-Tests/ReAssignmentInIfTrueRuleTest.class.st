Class {
	#name : 'ReAssignmentInIfTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAssignmentInIfTrueRuleTest >> testRule [

	| critiques |
	self class
		compile:
		'testMethod |var| 1 == 1 ifTrue: [var := 1] ifFalse: [var := 2]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #testMethod.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #testMethod) removeFromSystem ]
]

{ #category : 'tests' }
ReAssignmentInIfTrueRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'testMethod |var| var := 1 == 1 ifTrue: [1] ifFalse: [2]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #testMethod.
	self assertEmpty: critiques ] ensure: [
		(self class >> #testMethod) removeFromSystem ]
]
