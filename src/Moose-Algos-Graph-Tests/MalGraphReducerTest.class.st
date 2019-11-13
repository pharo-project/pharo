Class {
	#name : #MalGraphReducerTest,
	#superclass : #TestCase,
	#instVars : [
		'graphReducer'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #helpers }
MalGraphReducerTest >> assertNodes: aNodeList doesNotInclude: anotherNodeList [
	aNodeList 
		collect: #model
		thenDo: 
			[ :model | 
			self 
				deny: ((anotherNodeList collect: #model) includes: model)
				description: model asString , ' should not be referenced!' ] 
]

{ #category : #helpers }
MalGraphReducerTest >> mergedNodesIn: aNodeList [
	^ (aNodeList select: [ :node | node mergedNodes notNil ]) flatCollect: #mergedNodes
]

{ #category : #running }
MalGraphReducerTest >> setUp [
	super setUp.
	graphReducer := MalGraphReducer new
]

{ #category : #tests }
MalGraphReducerTest >> testComplexCycle2 [
	MalGraphFixture complexCycleGraph2: graphReducer.
	graphReducer run.
	self assert: graphReducer nodes size equals: 6.
	self assert: (graphReducer nodes at: 6) mergedNodes size equals: 4
]

{ #category : #tests }
MalGraphReducerTest >> testGraphReducingWhenNoCycle [
	| initialGraph |
	MalGraphFixture simpleGraph: graphReducer.
	initialGraph := graphReducer graph copy.
	graphReducer run.
	self assert: initialGraph equals: graphReducer graph
]

{ #category : #tests }
MalGraphReducerTest >> testMergedNodesNotReferencedInNextNodes [
	MalGraphFixture complexCycleGraph2: graphReducer.
	graphReducer run.

	self 
		assertNodes: (graphReducer nodes flatCollect: #nextNodes) 
		doesNotInclude: (self mergedNodesIn: graphReducer nodes)
]

{ #category : #tests }
MalGraphReducerTest >> testMergedNodesNotReferencedInPreviousNodes [
	MalGraphFixture complexCycleGraph2: graphReducer.
	graphReducer run.

	self 
		assertNodes: (graphReducer nodes flatCollect: #previousNodes) 
		doesNotInclude: (self mergedNodesIn: graphReducer nodes)

]

{ #category : #tests }
MalGraphReducerTest >> testNextNodesFor [
	| nodes |
	MalGraphFixture complexCycleGraph2: graphReducer.
	nodes := {(graphReducer findNode: $f) . (graphReducer findNode: $g)}.
	self
		assert: ((graphReducer nextNodesFor: nodes) collect: #model as: SortedCollection) asArray
		equals: #($a $b $d $e $i)
]

{ #category : #tests }
MalGraphReducerTest >> testNoCycle [
	MalGraphFixture complexCycleGraph2: graphReducer.
	graphReducer run.
	graphReducer nodes do:
		[ :node | 
		self 
			deny: ((node nextNodes includes: node) or: [ node previousNodes includes: node ])
			description: node asString, ' node is in cycle!' ]
]

{ #category : #tests }
MalGraphReducerTest >> testPreviousNodesFor [
	| nodes |
	MalGraphFixture complexCycleGraph2: graphReducer.
	nodes := {(graphReducer findNode: $f) . (graphReducer findNode: $g)}.
	self
		assert: ((graphReducer previousNodesFor: nodes) collect: #model as: SortedCollection) asArray
		equals: #($a $b $c)
]
