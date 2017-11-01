Class {
	#name : #MalWeightedEdge,
	#superclass : #MalGraphEdge,
	#instVars : [
		'weight'
	],
	#category : 'Moose-Algos-Graph'
}

{ #category : #accessing }
MalWeightedEdge >> asTuple [

	^ {from model. to model. weight}
]

{ #category : #accessing }
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
