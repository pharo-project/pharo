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
MalWeightedEdge >> printString [

	^ self from printString, ' -> ', self to printString, ' (', self weight printString, ')'
]

{ #category : #accessing }
MalWeightedEdge >> weight [
	^ weight
]

{ #category : #accessing }
MalWeightedEdge >> weight: anObject [
	weight := anObject
]
