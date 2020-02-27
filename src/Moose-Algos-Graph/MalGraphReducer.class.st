"
The reduced graph of G is the graph G where each strongly connected components was merged into one vertice / node.

circuitsNodes instance variable will collect all nodes created by merging nodes in a circuit.
"
Class {
	#name : #MalGraphReducer,
	#superclass : #MalGraphAlgorithm,
	#instVars : [
		'circuits',
		'circuitNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #updating }
MalGraphReducer >> addCircuitNodes [
	nodes addAll: circuitNodes
]

{ #category : #running }
MalGraphReducer >> findCircuits [
	| tarjanCircuits |
	tarjanCircuits := MalTarjan new
		nodesAndNextFrom: nodes;
		run;
		circuits.
	circuits := tarjanCircuits 
		collect: [ :circuit | 
			circuit collect: [ :each | self findNode: each model ] ]
]

{ #category : #initialization }
MalGraphReducer >> initialize [
	super initialize.
	circuitNodes := OrderedCollection new.
	circuits := OrderedCollection new
]

{ #category : #merging }
MalGraphReducer >> mergeNodesInCircuit: aNodeList [
	| sortedNodes |
	"ensure that we get the same order in nodes part of a cycle. MalTarjan uses a dictionary and order depends on model hash that can differ from a machine to another one."
	sortedNodes := aNodeList sort: [ :one :other | one model < other model ].
	circuitNodes add: (self nodeClass new
		mergedNodes: sortedNodes;
		model: (sortedNodes collect: #model);
		nextNodes: (self nextNodesFor: sortedNodes);
		previousNodes: (self previousNodesFor: sortedNodes);
		yourself)
]

{ #category : #merging }
MalGraphReducer >> mergeNodesInCircuits [
	circuits do: [ :each | self mergeNodesInCircuit: each ].
	self removeReferencesToMergedNodes.
	self removeMergedNodes.
	self addCircuitNodes
]

{ #category : #computing }
MalGraphReducer >> nextNodesFor: aNodeList [
	"compute next nodes for the future node replacing aNodeList"
	^ ((aNodeList flatCollect: #nextNodes as: Set)
		difference: aNodeList) asOrderedCollection
]

{ #category : #configuration }
MalGraphReducer >> nodeClass [
	^ MalReducedGraphNode
]

{ #category : #computing }
MalGraphReducer >> previousNodesFor: aNodeList [
	"compute previous nodes for the future node replacing aNodeList"
	^ ((aNodeList flatCollect: #previousNodes as: Set)
		difference: aNodeList) asOrderedCollection
]

{ #category : #updating }
MalGraphReducer >> removeCircularDependenciesFrom: circuitNode [
	circuitNode nextNodes removeAllSuchThat: [ :each | each = circuitNode ].
	circuitNode previousNodes removeAllSuchThat: [ :each | each = circuitNode ]
]

{ #category : #updating }
MalGraphReducer >> removeMergedNodes [
	nodes removeAll: (circuitNodes flatCollect: #mergedNodes)
]

{ #category : #updating }
MalGraphReducer >> removeReferencesToMergedNodes [
	circuitNodes do: 
		[ :circuitNode |
		self 
			updateReferencesTo: circuitNode mergedNodes
			by: circuitNode.
		self removeCircularDependenciesFrom: circuitNode ]
]

{ #category : #running }
MalGraphReducer >> run [
	self findCircuits.
	self mergeNodesInCircuits.
	^ self nodes
]

{ #category : #updating }
MalGraphReducer >> updateNextNodesOf: mergedNode replacing: aNodeList by: anotherNode [
	mergedNode nextNodes
		do: [ :node | 
			node previousNodes
				removeAllFoundIn: aNodeList;
				addIfNotPresent: anotherNode ]
]

{ #category : #updating }
MalGraphReducer >> updatePreviousNodesOf: mergedNode replacing: aNodeList by: anotherNode [
	mergedNode previousNodes
		do: [ :node | 
			node nextNodes
				removeAllFoundIn: aNodeList;
				addIfNotPresent: anotherNode ]
]

{ #category : #updating }
MalGraphReducer >> updateReferencesTo: aNodeList by: anotherNode [
	aNodeList do: 
		[ :mergedNode | 
		self 
			updatePreviousNodesOf: mergedNode
			replacing: aNodeList
			by: anotherNode.
		self 
			updateNextNodesOf: mergedNode
			replacing: aNodeList
			by: anotherNode ]
]
