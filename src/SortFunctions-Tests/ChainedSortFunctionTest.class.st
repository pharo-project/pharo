Class {
	#name : #ChainedSortFunctionTest,
	#superclass : #TestCase,
	#category : #SortFunctions-Tests
}

{ #category : #tests }
ChainedSortFunctionTest >> testComparisonOfFunctions [

	self assert: #name ascending, #surname equals: #name ascending, #surname.
	self assert: (#name ascending, #surname) hash equals: (#name ascending, #surname) hash.

	self deny: #name ascending, #surname equals: #name ascending, #surname2.
	self deny: #name ascending, #surname equals: #name descending, #surname.
	self deny: #name ascending, #surname ascending equals: #name ascending, #surname descending.
]

{ #category : #tests }
ChainedSortFunctionTest >> testEvaluation [

	| function |
	function := #first ascending , [:a | a at: 2] descending , #last ascending.
	"first different"
	self assert: (function value: #(1 2 3) value: #(2 2 3)).
	self deny: (function value: #(3 2 3) value: #(2 2 3)).
	"first the same"
	self assert: (function value: #(1 2 3) value: #(1 1 3)).
	self assert: (function value: #(1 2 2) value: #(1 1 3)).
	self assert: (function value: #(1 2 3) value: #(1 1 2)).
	self deny: (function value: #(1 2 3) value: #(1 3 3)).
	self deny: (function value: #(1 2 2) value: #(1 3 3)).
	self deny: (function value: #(1 2 3) value: #(1 3 2)).
	"first and second the same"
	self assert: (function value: #(1 2 3) value: #(1 2 3)).
	self assert: (function value: #(1 2 2) value: #(1 2 3)).
	self deny: (function value: #(1 2 4) value: #(1 2 3))
]
