"
I'm the common superclass for all graphs related algorithms.
I store edges and nodes and provides convenience methods to add, access and find nodes or edges.

Once configured, send the run message to execute the algorithm.
"
Class {
	#name : #MalGraphAlgorithm,
	#superclass : #Object,
	#instVars : [
		'nodes',
		'edges',
		'nodeSortBlock'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #private }
MalGraphAlgorithm >> addEdge: eModel from: source to: target [
	| edge sourceNode targetNode |
	sourceNode := self findNode: (source value: eModel) ifAbsent: [ ^ nil ].
	targetNode := self findNode: (target value: eModel) ifAbsent: [ ^ nil ].
	self edgeClass
		ifNil: [ sourceNode to: targetNode.
			targetNode from: sourceNode.
			^ nil ]
		ifNotNil: [ edge := self edgeClass with: eModel.
			sourceNode to: targetNode edge: edge.
			targetNode from: sourceNode edge: edge.
			edge from: sourceNode.
			edge to: targetNode.
			self edges add: edge.
			^ edge ]
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> addNodeFor: aModel [
	^ self 
		findNode: aModel
		ifAbsent: [ nodes add: (self nodeClass with: aModel) ]
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> addNodesFrom: model childrenBlock: childrenBlock [
	"recursively add nodes from the model"
	| parent |
	model ifNil: [ ^ self ].

	self
		findNode: model
		ifFound: [ ^ self "Prevent cycles. Do not add a node already added." ].

	parent := self addNodeFor: model.
	(childrenBlock value: model)
		do: [ :child | 
				self addNodesFrom: child childrenBlock: childrenBlock.
				self addEdge: { parent model . child } from: #first to: #second ]
]

{ #category : #testing }
MalGraphAlgorithm >> canUseSortedNodes [
	"New implementation of the algos to have better performances to find nodes"

	"If nodes are sorted following this block, the find method will be a lot faster. The block should be set on the run method and the nodes sorted at this moment"

	^ nodeSortBlock isNotNil
]

{ #category : #configuration }
MalGraphAlgorithm >> edgeClass [
	^ nil
]

{ #category : #accessing }
MalGraphAlgorithm >> edges [
	^ edges
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> edges: aCollection from: source to: target [
	aCollection 
		do: [ :eModel |	self addEdge: eModel from: source to: target ]
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> edges: aCollection from: source to: target weight: weightFunction [
	| edge |
	aCollection 
		do: [ :eModel |
			edge := self addEdge: eModel from: source to: target.
			edge ifNotNil: [edge weight: (weightFunction value: eModel)] ]
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> edges: aCollection from: source toAll: targets [
	aCollection
		do: [ :eModel | 
			(targets value: eModel)
				do: [ :target | 
					self
						addEdge:
							{(source value: eModel).
							target}
						from: #first
						to: #second ] ]
]

{ #category : #private }
MalGraphAlgorithm >> ensureNodesAreSorted [
	"New implementation of the algos to have better performances to find nodes"
	"If one change is made in the node list, one should call again sortNodes"

	nodeSortBlock ifNotNil: [ ^ self ].
	self nodeSortBlock: [ :a :b | a model hash <= b model hash ].
	self sortNodes
]

{ #category : #private }
MalGraphAlgorithm >> findBinaryBlock: aModel [
	"New implementation of the algos to have better performances to find nodes"
	^ [ :aNode | 
	aNode model = aModel
		ifTrue: [ 0 ]
		ifFalse: [ aModel hash < aNode model hash
				ifTrue: [ -1 ]
				ifFalse: [ 1 ] ] ]
]

{ #category : #accessing }
MalGraphAlgorithm >> findEdge: aModel [
	^ self edges detect: [:edge | edge model = aModel]
]

{ #category : #accessing }
MalGraphAlgorithm >> findNode: aModel [
	self canUseSortedNodes
		ifFalse: [ ^ self nodes detect: [ :aNode | aNode model = aModel ] ].
	^ self nodes findBinary: (self findBinaryBlock: aModel)
]

{ #category : #accessing }
MalGraphAlgorithm >> findNode: aModel ifAbsent: aBlock [
	self canUseSortedNodes
		ifFalse: [ ^ self nodes detect: [ :node | node model = aModel ] ifNone: aBlock ].
^ self nodes
		findBinary: (self findBinaryBlock: aModel) ifNone: aBlock
]

{ #category : #accessing }
MalGraphAlgorithm >> findNode: aModel ifFound: aBlock [
	self canUseSortedNodes
		ifFalse: [ ^ self nodes detect: [ :node | node model = aModel ] ifFound: aBlock ].
	"New implementation of the algos to have better performances to find nodes"
	self notYetImplemented
]

{ #category : #accessing }
MalGraphAlgorithm >> graph [
	^ {self nodes . self edges}
]

{ #category : #initialization }
MalGraphAlgorithm >> initialize [
	nodes := OrderedCollection new.
	edges := OrderedCollection new.
]

{ #category : #configuration }
MalGraphAlgorithm >> nodeClass [
	^ MalGraphNode
]

{ #category : #accessing }
MalGraphAlgorithm >> nodeSortBlock: aBlockClosure [ 
	nodeSortBlock := aBlockClosure
]

{ #category : #accessing }
MalGraphAlgorithm >> nodes [
	^ nodes
]

{ #category : #'building - graph' }
MalGraphAlgorithm >> nodes: aNodeList [
	aNodeList 
		do: [ :model |	self addNodeFor: model ]
]

{ #category : #adding }
MalGraphAlgorithm >> nodesFromSet: aSet [
	"Ensure that the collection do not contains any duplicated item"

	self assert: aSet class == Set.
	nodes addAll: (aSet collect: [ :aModel | self nodeClass with: aModel ])
]

{ #category : #private }
MalGraphAlgorithm >> privateEdges: aCollection [

	edges := aCollection 
]

{ #category : #private }
MalGraphAlgorithm >> privateNodes: aCollection [

	nodes := aCollection 
]

{ #category : #running }
MalGraphAlgorithm >> run [

	self subclassResponsibility 
]

{ #category : #private }
MalGraphAlgorithm >> sortNodes [
	self canUseSortedNodes ifFalse: [ ^self ].
	self nodes sort: nodeSortBlock.
]
