"
Node used by the MalReducedGraph algorithm.
It has an additional instance variable to track merged nodes.
"
Class {
	#name : #MalReducedGraphNode,
	#superclass : #MalNodeWithPreviousAndNext,
	#instVars : [
		'mergedNodes'
	],
	#category : #Moose-Algos-Graph
}

{ #category : #accessing }
MalReducedGraphNode >> label [
	^ ''
]

{ #category : #accessing }
MalReducedGraphNode >> mergedNodes [
	^ mergedNodes
]

{ #category : #setting }
MalReducedGraphNode >> mergedNodes: aNodeList [
	mergedNodes := aNodeList
]
