"
Default representation of a graph vertice (node).
"
Class {
	#name : #MalGraphNode,
	#superclass : #Object,
	#instVars : [
		'model'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #'instance creation' }
MalGraphNode class >> with: aModel [

	^ self new model: aModel
]

{ #category : #accessing }
MalGraphNode >> from: sourceNode [
]

{ #category : #accessing }
MalGraphNode >> from: sourceNode edge: anEdge [
]

{ #category : #accessing }
MalGraphNode >> label [

	^ 'N '
]

{ #category : #accessing }
MalGraphNode >> model [
	^ model
]

{ #category : #accessing }
MalGraphNode >> model: aModel [

	model := aModel
]

{ #category : #printing }
MalGraphNode >> printOn: stream [
	stream nextPutAll: self label.
	model printOn: stream
]

{ #category : #accessing }
MalGraphNode >> to: targetNode [
]

{ #category : #accessing }
MalGraphNode >> to: targetNode edge: anEdge [
]
