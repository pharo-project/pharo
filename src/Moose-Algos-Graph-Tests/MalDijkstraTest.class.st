Class {
	#name : #MalDijkstraTest,
	#superclass : #TestCase,
	#instVars : [
		'dijsktra'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalDijkstraTest >> setUp [
	super setUp.
	dijsktra := MalDijkstra new
]

{ #category : #tests }
MalDijkstraTest >> testAseBasicCircuit [
	MalGraphFixture aseCircuitGraph: dijsktra.

	self assert: 1 equals: (dijsktra runFrom: $a to: $b).
	dijsktra reset.
	self assert: 3 equals: (dijsktra runFrom: $a to: $e).
	dijsktra reset.
	self assert: 2 equals: (dijsktra runFrom: $c to: $b).
	dijsktra reset.
	self assert: 5 equals: (dijsktra runFrom: $a to: $h).
	dijsktra reset.
	self assert: Float infinity equals: (dijsktra runFrom: $h to: $a)
]

{ #category : #tests }
MalDijkstraTest >> testAseBasicCircuitBacktrack [
	MalGraphFixture aseCircuitGraph: dijsktra.

	self assert: (#($a $b) hasEqualElements: (dijsktra runFrom: $a to: $b; backtrack)).
	dijsktra reset.
	self assert: (#($a $b $d $e) hasEqualElements: (dijsktra runFrom: $a to: $e; backtrack)).
	dijsktra reset.
	self assert: (#($c $d $b) hasEqualElements: (dijsktra runFrom: $c to: $b; backtrack)).
	dijsktra reset.
	self assert: (#($a $b $c $f $g $h) hasEqualElements: (dijsktra runFrom: $a to: $h; backtrack)).
	dijsktra reset.
	self assert: (#() hasEqualElements: (dijsktra runFrom: $h to: $a; backtrack))
]
