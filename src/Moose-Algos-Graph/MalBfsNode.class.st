Class {
	#name : #MalBfsNode,
	#superclass : #MalGraphNode,
	#traits : 'MalTEdgeNode - {#previousEdges. #from:edge:}',
	#classTraits : 'MalTEdgeNode classTrait',
	#instVars : [
		'nextEdges',
		'firstDepth'
	],
	#category : #Moose-Algos-Graph
}

{ #category : #accessing }
MalBfsNode >> firstDepth [
	^ firstDepth
]

{ #category : #accessing }
MalBfsNode >> firstDepth: anObject [
	firstDepth := anObject
]

{ #category : #initialization }
MalBfsNode >> initialize [ 

	nextEdges := OrderedCollection new.

]

{ #category : #accessing }
MalBfsNode >> nextEdges [

	^ nextEdges 
]

{ #category : #setting }
MalBfsNode >> notVisited [

	firstDepth := nil
]

{ #category : #testing }
MalBfsNode >> visited [

	^ firstDepth notNil
]
