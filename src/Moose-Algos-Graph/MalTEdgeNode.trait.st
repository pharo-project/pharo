Trait {
	#name : #MalTEdgeNode,
	#category : #'Moose-Algos-Graph'
}

{ #category : #edges }
MalTEdgeNode >> from: aNode edge: anEdge [

	self previousEdges add: anEdge
]

{ #category : #edges }
MalTEdgeNode >> nextEdges [

	self explicitRequirement
]

{ #category : #edges }
MalTEdgeNode >> previousEdges [

	self explicitRequirement
]

{ #category : #edges }
MalTEdgeNode >> to: aNode edge: anEdge [

	self nextEdges add: anEdge
]
