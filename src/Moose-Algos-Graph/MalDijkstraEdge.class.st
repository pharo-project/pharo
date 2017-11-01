Class {
	#name : #MalDijkstraEdge,
	#superclass : #MalWeightedEdge,
	#instVars : [
		'visited'
	],
	#category : 'Moose-Algos-Graph'
}

{ #category : #initialization }
MalDijkstraEdge >> initialize [ 

	super initialize.
	weight := 1.
	visited := false
]

{ #category : #accessing }
MalDijkstraEdge >> isVisited [

	^ visited
]

{ #category : #accessing }
MalDijkstraEdge >> visited: aBoolean [

	visited := aBoolean 
]
