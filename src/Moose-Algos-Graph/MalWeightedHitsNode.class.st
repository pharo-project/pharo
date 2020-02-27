Class {
	#name : #MalWeightedHitsNode,
	#superclass : #MalHitsNode,
	#traits : 'MalTEdgeNode',
	#classTraits : 'MalTEdgeNode classTrait',
	#instVars : [
		'nextEdges',
		'previousEdges'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalWeightedHitsNode >> label [

	^ 'WH '
]

{ #category : #accessing }
MalWeightedHitsNode >> nextEdges [

	^ nextEdges
]

{ #category : #accessing }
MalWeightedHitsNode >> previousEdges [

	^ previousEdges
]
