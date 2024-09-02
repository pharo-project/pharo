Class {
	#name : 'ReToDoWithIncrementRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReToDoWithIncrementRuleTest >> testRule [

	| critiques |
	self class
		compile:
		'method 1 to: 100 do: [ :e | | counter | counter:= 1 .counter := counter + 1 ]  '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReToDoWithIncrementRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile:
		'method | counter | counter := 1. 1 to: 100 do: [ counter := counter + 1 ]  '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReToDoWithIncrementRuleTest >> testRuleNotViolatedWithBy [

	| critiques |
	self class
		compile:
		'method | counter | counter := 1. 1 to: 100 by: 3 do: [ counter := counter + 1 ]  '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReToDoWithIncrementRuleTest >> testRuleWithBy [ 

	| critiques |
	self class
		compile:
		'method 1 to: 100 by: 3 do: [ :e | | counter | counter:= 1 .counter := counter + 3 ]  '
		classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
