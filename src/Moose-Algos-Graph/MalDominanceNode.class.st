Class {
	#name : #MalDominanceNode,
	#superclass : #MalDisjointSetNode,
	#instVars : [
		'previousNodes',
		'uniqueSource'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalDominanceNode >> dominantNode [

	^ self find
]

{ #category : #accessing }
MalDominanceNode >> firstSource [

	^ previousNodes first
]

{ #category : #accessing }
MalDominanceNode >> flagUnique [

	uniqueSource := true
]

{ #category : #accessing }
MalDominanceNode >> from: sourceNode [

	previousNodes add: sourceNode 
]

{ #category : #accessing }
MalDominanceNode >> hasUniqueSource [
	^ self previousNodes size = 1
]

{ #category : #initialization }
MalDominanceNode >> initialize [
	super initialize.
	previousNodes := OrderedCollection new.
	uniqueSource := false
]

{ #category : #accessing }
MalDominanceNode >> isDominant [
	"A node is dominant if it has been flagged as the unique Source by one of its target
	 AND it is its own parent in the partition"
	^ uniqueSource and: [self find = self]
]

{ #category : #accessing }
MalDominanceNode >> isDominated [

	^ self isDominant not and: [self find isDominant]
]

{ #category : #accessing }
MalDominanceNode >> label [

	^ 'Dm '
]

{ #category : #accessing }
MalDominanceNode >> previousNodes [
	^ previousNodes
]
