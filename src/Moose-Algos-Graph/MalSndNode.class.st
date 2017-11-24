Class {
	#name : #MalSndNode,
	#superclass : #MalNodeWithPreviousAndNext,
	#instVars : [
		'visited',
		'bfsAncestor'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalSndNode >> bfsAncestor [
	^ bfsAncestor
]

{ #category : #accessing }
MalSndNode >> bfsAncestor: anObject [
	bfsAncestor := anObject
]

{ #category : #accessing }
MalSndNode >> visited [
	^ visited
]

{ #category : #accessing }
MalSndNode >> visited: anObject [
	visited := anObject
]
