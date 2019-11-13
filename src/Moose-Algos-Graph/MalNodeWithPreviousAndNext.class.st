"
A basic node able to host the model and to represent edges with no additional class.
Edges are navigable in both way through the nextNodes and prevousNodes instances variables.
"
Class {
	#name : #MalNodeWithPreviousAndNext,
	#superclass : #MalNodeWithNext,
	#instVars : [
		'previousNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalNodeWithPreviousAndNext >> from: sourceNode [
	self previousNodes add: sourceNode 
]

{ #category : #initialization }
MalNodeWithPreviousAndNext >> initialize [
	super initialize.
	previousNodes := OrderedCollection new
]

{ #category : #testing }
MalNodeWithPreviousAndNext >> isRoot [
	^ self previousNodes isEmpty
]

{ #category : #accessing }
MalNodeWithPreviousAndNext >> previousNodes [
	^ previousNodes
]

{ #category : #setting }
MalNodeWithPreviousAndNext >> previousNodes: aNodeList [
	previousNodes := aNodeList
]
