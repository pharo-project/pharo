Class {
	#name : 'ReTempsFormattingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReTempsFormattingRuleTest >> testRuleNotViolated [ 

	| critiques |
	
	self class compile: 'valid | a b | a := 1. b := 2. ^ a + b' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #valid.
	self assertEmpty: critiques ] ensure: [ (self class >> #valid) removeFromSystem ]
]

{ #category : 'tests' }
ReTempsFormattingRuleTest >> testRuleWithManySpace [

	| critiques |
	self class compile: 'invalid1 |  a b | a := 1. b := 2. ^ a + b' classified: 'test-helper'.
	self class compile: 'invalid2 | a b  | a := 1. b := 2. ^ a + b' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #invalid1.
	critiques add: (self myCritiquesOnMethod: self class >> #invalid2).
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #invalid1) removeFromSystem.
		(self class >> #invalid2) removeFromSystem ]
]

{ #category : 'tests' }
ReTempsFormattingRuleTest >> testRuleWithSpaceBetweenTemps [

	| critiques |
	self class compile: 'invalid | a  b | a := 1. b := 2. ^ a + b' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #invalid.
	self assert: critiques size equals: 1 ] ensure: [
		(self class >> #invalid) removeFromSystem. ]
]

{ #category : 'tests' }
ReTempsFormattingRuleTest >> testRuleWithoutSpace [

	| critiques |
	self class compile: 'invalid1 | a| a:=1. ^a' classified: 'test-helper'.
	self class compile: 'invalid2 |b | b:=1. ^b' classified: 'test-helper'.
	[
	critiques := self myCritiquesOnMethod: self class >> #invalid1.
	critiques add: (self myCritiquesOnMethod: self class >> #invalid2).
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #invalid1) removeFromSystem.
		(self class >> #invalid2) removeFromSystem ]
]
