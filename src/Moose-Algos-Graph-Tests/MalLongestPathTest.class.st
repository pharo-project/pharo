Class {
	#name : #MalLongestPathTest,
	#superclass : #TestCase,
	#instVars : [
		'longestPath'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #helpers }
MalLongestPathTest >> assert: aCollectionOfNodesModel hasWeight: values [
	self assert: aCollectionOfNodesModel size equals: values size.
	aCollectionOfNodesModel
		withIndexDo: [ :element :index | self assert: (longestPath findNode: element) pathWeight equals: (values at: index) ]
]

{ #category : #running }
MalLongestPathTest >> setUp [
	super setUp.
	longestPath := MalLongestPath new
]

{ #category : #tests }
MalLongestPathTest >> testIsCompletedInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath computeLeafNodes.
	self deny: longestPath isCompleted
]

{ #category : #tests }
MalLongestPathTest >> testLeafNodesInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath computeLeafNodes.
	self assert: (longestPath leafNodes collect: #model) equals: #($c) asOrderedCollection
]

{ #category : #tests }
MalLongestPathTest >> testRootNodesInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath computeRootNodes.
	self assert: (longestPath rootNodes collect: #model) equals: #($a $d) asOrderedCollection
]

{ #category : #tests }
MalLongestPathTest >> testRootNodesInWithoutCyclesMediumGraph [
	MalGraphFixture withoutCyclesMediumGraph: longestPath.
	longestPath computeRootNodes.
	self assert: (longestPath rootNodes collect: #model) equals: #($a $b) asOrderedCollection
]

{ #category : #tests }
MalLongestPathTest >> testRootNodesRankIsZeroInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath initializeRootNodes.
	self assert: (longestPath rootNodes collect: #model) equals: #($a $d) asOrderedCollection.
	self assert: (longestPath rootNodes collect: #pathWeight) equals: #(0 0) asOrderedCollection
]

{ #category : #tests }
MalLongestPathTest >> testRunInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath run.
	self assert: ($a to: $d) hasWeight: #(0 1 2 0)
]

{ #category : #tests }
MalLongestPathTest >> testRunInWithoutCyclesComplexGraph [
	MalGraphFixture withoutCyclesComplexGraph: longestPath.
	longestPath run.
	self assert: ($a to: $s) hasWeight: #(0 0 1 2 1 3 1 1 4 0 1 1 2 2 2 3 2 5 6)
]

{ #category : #tests }
MalLongestPathTest >> testRunInWithoutCyclesMediumGraph [
	MalGraphFixture withoutCyclesMediumGraph: longestPath.
	longestPath run.
	self assert: ($a to: $h) hasWeight: #(0 0 1 2 1 3 1 1)
]

{ #category : #tests }
MalLongestPathTest >> testRunIsCompletedInSimpleGraph [
	MalGraphFixture simpleGraph: longestPath.
	longestPath run.
	self assert: longestPath isCompleted
]
