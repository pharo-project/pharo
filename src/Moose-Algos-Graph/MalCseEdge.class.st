Class {
	#name : #MalCseEdge,
	#superclass : #MalDijkstraEdge,
	#instVars : [
		'cseVisited'
	],
	#category : 'Moose-Algos-Graph'
}

{ #category : #accessing }
MalCseEdge >> cseVisited [
	^ cseVisited
]

{ #category : #accessing }
MalCseEdge >> cseVisited: aBoolean [
	cseVisited := aBoolean
]

{ #category : #initialization }
MalCseEdge >> initialize [ 

	super initialize.
	self cseVisited: false.
]
