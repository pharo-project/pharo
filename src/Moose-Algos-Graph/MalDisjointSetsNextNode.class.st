Class {
	#name : #MalDisjointSetsNextNode,
	#superclass : #MalDisjointSetNode,
	#instVars : [
		'nextNodes'
	],
	#category : #Moose-Algos-Graph
}

{ #category : #accessing }
MalDisjointSetsNextNode >> initialize [

	self flag: #refactor.
	self makeSet. "should be called in super initialize"
	nextNodes := OrderedCollection new
]

{ #category : #accessing }
MalDisjointSetsNextNode >> nextNodes [

	^ nextNodes 
]

{ #category : #adding }
MalDisjointSetsNextNode >> to: targetNode [
	self nextNodes add: targetNode
]
