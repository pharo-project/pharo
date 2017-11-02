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
		'currentRank',
		'rootNodes',
		'leafNodes',
		'maxIterations'
	],
	#category : 'Moose-Algos-Graph'
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
	| nodesToManage |
	self isCompleted
		ifFalse: [ 
			currentRank := OrderedCollection new.
			nodesToManage := self nodes reject: [ :node | previousRanks includes: node ].
			nodesToManage
				do: [ :node | 
					(previousRanks includesAll: node previousNodes)
						ifTrue: [ 
							currentRank add: node.
							node pathWeight: (node previousNodes max: [ :maxNode | maxNode pathWeight ]) + 1 ] ].
			previousRanks addAll: currentRank.
			maxIterations := maxIterations -1.
			self computeStep ]
]

{ #category : #configuration }
MalLongestPath >> edgeClass [
	^ MalGraphEdge
]

{ #category : #initialize-release }
MalLongestPath >> initialize [
	super initialize.
	previousRanks := OrderedCollection new
]

{ #category : #initialize-release }
MalLongestPath >> initializeRootNodes [
	self computeRootNodes.
	self setRanks: self rootNodes at: 0
]

{ #category : #testing }
MalLongestPath >> isCompleted [
	maxIterations = 0
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
