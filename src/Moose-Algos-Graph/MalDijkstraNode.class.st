Class {
	#name : #MalDijkstraNode,
	#superclass : #MalGraphNode,
	#traits : 'MalTEdgeNode',
	#classTraits : 'MalTEdgeNode classTrait',
	#instVars : [
		'pathWeight',
		'previousEdges',
		'nextEdges',
		'previousNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalDijkstraNode >> from: aNode edge: anEdge [

	self previousNodes add: aNode.
	self previousEdges add: anEdge
]

{ #category : #initialization }
MalDijkstraNode >> initialize [

	previousNodes := OrderedCollection new.
	previousEdges := OrderedCollection new.
	nextEdges := OrderedCollection new.
	pathWeight := Float infinity "initialization for Dijsktra"
]

{ #category : #accessing }
MalDijkstraNode >> nextEdges [

	^ nextEdges
]

{ #category : #accessing }
MalDijkstraNode >> pathWeight [
	^ pathWeight
]

{ #category : #accessing }
MalDijkstraNode >> pathWeight: anObject [
	pathWeight := anObject
]

{ #category : #accessing }
MalDijkstraNode >> previousEdges [

	^ previousEdges
]

{ #category : #accessing }
MalDijkstraNode >> previousNodes [

	^ previousNodes
]

{ #category : #printing }
MalDijkstraNode >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' - ';
		nextPutAll: pathWeight asString
]
