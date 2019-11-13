Class {
	#name : #MalHalNode,
	#superclass : #MalTarjanNode,
	#instVars : [
		'hNumber',
		'previousNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalHalNode >> from: aHalNode [
	previousNodes add: aHalNode
]

{ #category : #accessing }
MalHalNode >> hNumber [
	^ hNumber
]

{ #category : #accessing }
MalHalNode >> hNumber: anObject [
	hNumber := anObject
]

{ #category : #initialization }
MalHalNode >> initialize [
	super initialize.
	previousNodes := Set new
]

{ #category : #testing }
MalHalNode >> isSink [

	^ self nextNodes isEmpty 
]

{ #category : #testing }
MalHalNode >> isSource [

	^ self previousNodes isEmpty 
]

{ #category : #accessing }
MalHalNode >> label [

	^ 'Ha '
]

{ #category : #'hal-algorithm' }
MalHalNode >> markCycleWith: anInteger [

	self cycleNodes do: [ :next | next hNumber: anInteger ].
	^ self cycleNodes 
]

{ #category : #'hal-algorithm' }
MalHalNode >> markHNumber: anInteger [

	self hNumber: anInteger.
	self isInCycle ifTrue: [
		^ self markCycleWith: anInteger
	].
	^ OrderedCollection new
]

{ #category : #accessing }
MalHalNode >> previousNodes [
	^ previousNodes
]

{ #category : #accessing }
MalHalNode >> previousNodes: anObject [
	previousNodes := anObject
]

{ #category : #'hal-algorithm' }
MalHalNode >> removeCycleFromEdges [

	self nextNodes removeAllFoundIn: self cycleNodes.
	self previousNodes removeAllFoundIn: self cycleNodes 
]

{ #category : #accessing }
MalHalNode >> to: aNode [
	super to: aNode.
	aNode previousNodes add: self
]
