Class {
	#name : #MalDisjointSetNode,
	#superclass : #MalGraphNode,
	#instVars : [
		'parent'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalDisjointSetNode >> find [
	^ self = parent ifTrue: [ self ] ifFalse: [ parent find ]
]

{ #category : #accessing }
MalDisjointSetNode >> label [

	^ 'DSN '
]

{ #category : #accessing }
MalDisjointSetNode >> makeSet [
	parent := self
]

{ #category : #'private-accessing' }
MalDisjointSetNode >> parent [
	^ parent
]

{ #category : #'private-accessing' }
MalDisjointSetNode >> parent: anObject [
	parent := anObject
]

{ #category : #accessing }
MalDisjointSetNode >> union: aDSNode [

	aDSNode find parent: self find
]
