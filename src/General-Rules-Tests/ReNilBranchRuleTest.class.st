Class {
	#name : 'ReNilBranchRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'method 1 < 0 ifTrue: [ 1 ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleWithIfFalse [

	| critiques |
	self class
		compile: 'method 1 < 0 ifFalse: [ 1 ] ifTrue: [ nil ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleWithIfTrue [

	| critiques |
	self class
		compile: 'method 1 < 0 ifTrue: [ 1 ] ifFalse: [ nil ]'
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #method) removeFromSystem ]
]
