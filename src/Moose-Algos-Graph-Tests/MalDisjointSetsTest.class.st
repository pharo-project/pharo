Class {
	#name : #MalDisjointSetsTest,
	#superclass : #TestCase,
	#instVars : [
		'ds'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalDisjointSetsTest >> setUp [
	super setUp.
	ds := MalDisjointSets new
]

{ #category : #tests }
MalDisjointSetsTest >> test1Component [
	| components |
	ds nodes: (1 to: 5).
	ds edges: {(1 -> 2) . (2 -> 3) . (4 -> 5) . (1 -> 5)} from: #key to: #value.
	ds run.
	components := ds components.

	self assert: 1 equals: components size.
	self assert: ((1 to: 5) hasEqualElements: components first)
]

{ #category : #tests }
MalDisjointSetsTest >> test2Components [
	| components twoNodes threeNodes |
	ds nodes: (1 to: 5).
	ds edges: {(1 -> 2) . (2 -> 3) . (4 -> 5)} from: #key to: #value.
	ds run.
	components := ds components.

	self assert: 2 equals: components size.
	twoNodes := components detect: [ :c | c size = 2 ].
	threeNodes := components detect: [ :c | c size = 3 ].
	self assert: ((1 to: 3) hasEqualElements: threeNodes).
	self assert: ((4 to: 5) hasEqualElements: twoNodes)
]

{ #category : #tests }
MalDisjointSetsTest >> test3Components [
	| components twoNodes threeNodes singleNodeC |
	ds nodes: (1 to: 6).
	ds edges: {(1 -> 2) . (2 -> 3) . (4 -> 5)} from: #key to: #value.
	ds run.
	components := ds components.

	self assert: 3 equals: components size.
	twoNodes := components detect: [ :c | c size = 2 ].
	threeNodes := components detect: [ :c | c size = 3 ].
	singleNodeC := components detect: [ :c | c size = 1 ].
	self assert: ((1 to: 3) hasEqualElements: threeNodes).
	self assert: ((4 to: 5) hasEqualElements: twoNodes).
	self assert: 6 equals: singleNodeC anyOne
]
