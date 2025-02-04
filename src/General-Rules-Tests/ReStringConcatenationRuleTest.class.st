Class {
	#name : 'ReStringConcatenationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReStringConcatenationRuleTest >> testRule [

	| critiques |
	
	self class compile: 'method #(1 2 3) do: [ :each | string := string, each asString].' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReStringConcatenationRuleTest >> testRuleNotViolated [

	| critiques |
	
	self class compile: 'method String streamContents: [:s | #(1 2 3)  do: [:each | s nextPutAll: each asString]]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
