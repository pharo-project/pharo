"
A basic node able to host the model and to represent edges with no additional class.
Edges are navigable in one way through the nextNodes instance variable.
"
Class {
	#name : #MalNodeWithNext,
	#superclass : #MalGraphNode,
	#instVars : [
		'nextNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #initialization }
MalNodeWithNext >> initialize [
	super initialize.
	nextNodes := OrderedCollection new
]

{ #category : #testing }
MalNodeWithNext >> isLeaf [
	^ self nextNodes isEmpty
]

{ #category : #accessing }
MalNodeWithNext >> nextNodes [
	^ nextNodes
]

{ #category : #setting }
MalNodeWithNext >> nextNodes: aNodeList [
	nextNodes := aNodeList
]

{ #category : #accessing }
MalNodeWithNext >> to: targetNode [
	self nextNodes add: targetNode 
]
