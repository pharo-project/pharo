"
A MalLongestPath is the implementation of the longest path algo.
This is working only on graph not weighted and without circuits

See tests for more informations

"
Class {
	#name : #MalLongestPath,
	#superclass : #MalGraphAlgorithm,
	#instVars : [
		'previousRanks',
		'rootNodes',
		'leafNodes',
		'maxIterations'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #computing }
MalLongestPath >> computeLeafNodes [
	self leafNodes: (self nodes select: [ :node | node nextEdges isEmpty ])
]

{ #category : #computing }
MalLongestPath >> computeRootNodes [
	self rootNodes: (self nodes select: [ :node | node previousEdges isEmpty ])
]

{ #category : #computing }
MalLongestPath >> computeStep [
	| currentRank |
	self isCompleted ifTrue: [ ^ self ].
	currentRank := OrderedCollection new.
	(self nodes difference: previousRanks)
		select: [ :node | previousRanks includesAll: node previousNodes ]
		thenDo: [ :node | 
			currentRank add: node.
			node pathWeight: (node previousNodes max: [ :maxNode | maxNode pathWeight ]) + 1 ].
	previousRanks addAll: currentRank.
	maxIterations := maxIterations - 1.
	self computeStep
]

{ #category : #configuration }
MalLongestPath >> edgeClass [
	^ MalGraphEdge
]

{ #category : #initialization }
MalLongestPath >> initialize [
	super initialize.
	maxIterations := -1.
	previousRanks := OrderedCollection new
]

{ #category : #initialization }
MalLongestPath >> initializeRootNodes [
	self computeRootNodes.
	self setRanks: self rootNodes at: 0
]

{ #category : #testing }
MalLongestPath >> isCompleted [
	maxIterations isZero
		ifTrue: [ self error: 'Algorithm should be finished by now...'.
			^ true ].
	^ (self leafNodes anySatisfy: [ :node | node pathWeight = Float infinity ]) not
]

{ #category : #accessing }
MalLongestPath >> leafNodes [
	^ leafNodes
	
]

{ #category : #accessing }
MalLongestPath >> leafNodes: aCollectionOfNodes [
	leafNodes := aCollectionOfNodes
]

{ #category : #configuration }
MalLongestPath >> nodeClass [
	^ MalDijkstraNode
]

{ #category : #accessing }
MalLongestPath >> rootNodes [
	^ rootNodes 
	
]

{ #category : #accessing }
MalLongestPath >> rootNodes: aCollectionOfNodes [
	rootNodes := aCollectionOfNodes 
	
]

{ #category : #running }
MalLongestPath >> run [
	self ensureNodesAreSorted.
	self initializeRootNodes.
	self computeLeafNodes.
	previousRanks addAll: self rootNodes.
	maxIterations := self nodes size + 20.
	self computeStep
]

{ #category : #setting }
MalLongestPath >> setRanks: collectionOfNodes at: aRank [
	collectionOfNodes do: [ :aNode | aNode pathWeight: aRank ]
	
]
