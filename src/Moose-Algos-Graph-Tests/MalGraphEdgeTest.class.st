Class {
	#name : #MalGraphEdgeTest,
	#superclass : #TestCase,
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #tests }
MalGraphEdgeTest >> testPrintString [
	self
		assert: (MalGraphEdge with: 'model' from: 'from' to: 'to') printString
		equals:  '''from'' -> ''to'''.
]
