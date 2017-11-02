Trait {
	#name : #MalTEdgeNode,
	#category : 'Moose-Algos-Graph'
}

{ #category : #'as yet unclassified' }
MalTEdgeNode >> from: aNode edge: anEdge [

	self previousEdges add: anEdge
]

{ #category : #'as yet unclassified' }
MalTEdgeNode >> nextEdges [

	self explicitRequirement
]

{ #category : #'as yet unclassified' }
MalTEdgeNode >> previousEdges [

	self explicitRequirement
]

{ #category : #'as yet unclassified' }
MalTEdgeNode >> to: aNode edge: anEdge [

	self nextEdges add: anEdge
]
