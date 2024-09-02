Class {
	#name : 'ReAllAnyNoneSatisfyRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReAllAnyNoneSatisfyRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'method 
		
		^ (OrderedCollection new) noneSatisfy: [ :each | each == 1 ]'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAllAnyNoneSatisfyRuleTest >> testRuleWithAllSatisfy [

	| critiques |
	self class
		compile: 'method 
		
		(OrderedCollection new) do: [ :each | each == 1 ifFalse: [ ^ false ] ]. ^ true '
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAllAnyNoneSatisfyRuleTest >> testRuleWithAnySatisfy [

	| critiques |
	self class
		compile: 'method 
		
		(OrderedCollection new) do: [ :each | each == 1 ifTrue: [ ^ true ] ]. ^ false '
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAllAnyNoneSatisfyRuleTest >> testRuleWithNoneSatisfy [

	| critiques |
	self class
		compile: 'method 
		
		(OrderedCollection new) do: [ :each | each == 1 ifTrue: [ ^ false ] ]. ^ true '
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
