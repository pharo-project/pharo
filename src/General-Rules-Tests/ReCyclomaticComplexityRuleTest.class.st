Class {
	#name : 'ReCyclomaticComplexityRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReCyclomaticComplexityRuleTest >> testRule [

	| critiques |
	self class
		compile:
		'method 
		1 < 2 ifTrue: [ 2 < 3 ifTrue: [ 3 < 4 ifTrue: [ 4 < 5 ifTrue: [ 3 ] ] ifFalse: [ 1 ] ] ifFalse: [ 2 < 4 ifTrue: [ 1 ] ifFalse: [ 2 ] ] ] ifFalse: [ 1 < 3 ifTrue: [ 1 ] 		ifFalse: [ 2 ] ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCyclomaticComplexityRuleTest >> testRuleNotViolated [ 

	| critiques |
	self class
		compile:
		'method 
		1 < 2 ifTrue: [ 2 < 3 ifTrue: [ 3 < 4 ifTrue: [ 2 ] ifFalse: [ 1 ] ] ifFalse: [ 2 < 4 ifTrue: [ 1 ] ifFalse: [ 2 ] ] ] ifFalse: [ 1 < 3 ifTrue: [ 1 ] ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]
