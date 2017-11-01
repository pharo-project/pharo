Class {
	#name : #MalWeightedHits,
	#superclass : #MalHits,
	#category : 'Moose-Algos-Graph'
}

{ #category : #running }
MalWeightedHits >> computeAuthoritiesFor: aNode [

	aNode auth: (aNode previousEdges inject: 0 into: [:s :e| s + (e weight * e from hub)])
]

{ #category : #running }
MalWeightedHits >> computeHubsFor: aNode [

	aNode hub: (aNode nextEdges inject: 0 into: [:s :e| s + (e weight * e to auth)])
]

{ #category : #configuration }
MalWeightedHits >> edgeClass [

	^ MalWeightedEdge
]

{ #category : #configuration }
MalWeightedHits >> nodeClass [

	^ MalWeightedHitsNode 
]
