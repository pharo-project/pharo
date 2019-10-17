Class {
	#name : #MalWeightedEdgeTest,
	#superclass : #TestCase,
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #tests }
MalWeightedEdgeTest >> testPrintString [
	|edge|
	edge := MalWeightedEdge with: 'model' from: 'from' to: 'to'.
	edge weight: 7.
	self assert: edge printString equals: '''from'' -> ''to'' (7)'.
]
