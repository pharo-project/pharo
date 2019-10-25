Class {
	#name : #MalSccEdgeDecomposition,
	#superclass : #MalSccExtractor,
	#traits : 'MalTNormalize',
	#classTraits : 'MalTNormalize classTrait',
	#instVars : [
		'bfsp'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #running }
MalSccEdgeDecomposition >> bfsPathFrom: startNode to: endNode [
	^ bfsp
		start: startNode;
		end: endNode;
		run
]

{ #category : #configuration }
MalSccEdgeDecomposition >> edgeClass [
	^ MalGraphEdge 
]

{ #category : #initialization }
MalSccEdgeDecomposition >> initialize [ 
	super initialize.
	bfsp := MalBreadthFirstSearchPath new.

]

{ #category : #configuration }
MalSccEdgeDecomposition >> nodeClass [
	^ MalBfsNode 
]

{ #category : #running }
MalSccEdgeDecomposition >> run [
	bfsp 
		privateNodes: self nodes;
		privateEdges: self edges.
	self edges do: [:e||c|
		c := self 
			bfsPathFrom: e to 
			to: e from.
		self addCircuit: (self normalize: c) ]
]
