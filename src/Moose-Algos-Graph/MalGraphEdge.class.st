Class {
	#name : #MalGraphEdge,
	#superclass : #Object,
	#instVars : [
		'model',
		'from',
		'to'
	],
	#category : 'Moose-Algos-Graph'
}

{ #category : #'as yet unclassified' }
MalGraphEdge class >> with: aModel [

	^ self new model: aModel
]

{ #category : #'instance creation' }
MalGraphEdge class >> with: aModel from: srcNode to: dstNode [
	| edge |
	edge := self new.
	edge model: aModel.
	edge from: srcNode.
	edge to: dstNode.
	^ edge.
]

{ #category : #accessing }
MalGraphEdge >> asTuple [

	^ {from model. to model}
]

{ #category : #accessing }
MalGraphEdge >> from [
	^ from
]

{ #category : #accessing }
MalGraphEdge >> from: anObject [
	from := anObject
]

{ #category : #accessing }
MalGraphEdge >> model [
	^ model
]

{ #category : #accessing }
MalGraphEdge >> model: aModel [
	
	model := aModel
]

{ #category : #accessing }
MalGraphEdge >> printOn: aStream [
	self from printOn: aStream.
	aStream << ' -> '.
	self to printOn: aStream
]

{ #category : #accessing }
MalGraphEdge >> printString [

	^ self from printString, ' -> ', self to printString
]

{ #category : #accessing }
MalGraphEdge >> to [
	^ to
]

{ #category : #accessing }
MalGraphEdge >> to: anObject [
	to := anObject
]
