Class {
	#name : #MalDisjointSetsNextNode,
	#superclass : #MalDisjointSetNode,
	#instVars : [
		'nextNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #initialization }
MalDisjointSetsNextNode >> initialize [
	super initialize.
	self makeSet.
	nextNodes := OrderedCollection new
]

{ #category : #accessing }
MalDisjointSetsNextNode >> nextNodes [

	^ nextNodes 
]

{ #category : #accessing }
MalDisjointSetsNextNode >> to: targetNode [
	self nextNodes add: targetNode
]
