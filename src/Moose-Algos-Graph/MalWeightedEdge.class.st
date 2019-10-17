"
I represent a weighted edge in a graph. 

Public API and Key Messages
- weight
 
Instance Variables
	weight: <Number>
"
Class {
	#name : #MalWeightedEdge,
	#superclass : #MalGraphEdge,
	#instVars : [
		'weight'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalWeightedEdge >> asTuple [

	^ {from model. to model. weight}
]

{ #category : #printing }
MalWeightedEdge >> printOn: aStream [
	self from printOn: aStream.
	aStream nextPutAll: ' -> '.
	self to printOn: aStream.
	aStream nextPutAll: ' ('.
	self weight printOn: aStream.
	aStream nextPut: $).
]

{ #category : #accessing }
MalWeightedEdge >> weight [
	^ weight
]

{ #category : #accessing }
MalWeightedEdge >> weight: anObject [
	weight := anObject
]
