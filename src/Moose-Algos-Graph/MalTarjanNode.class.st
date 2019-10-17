"
Node type used by the MalTarjan algorithm.
Edges are not representated as full objects. We just keep a list of next nodes (following described directed edges).
"
Class {
	#name : #MalTarjanNode,
	#superclass : #MalNodeWithNext,
	#instVars : [
		'index',
		'lowlink',
		'inStack',
		'cycleNodes'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalTarjanNode >> cycleNodes [
	^ cycleNodes
]

{ #category : #setting }
MalTarjanNode >> cycleNodes: anObject [
	cycleNodes := anObject copy
		remove: self ;
		yourself
]

{ #category : #accessing }
MalTarjanNode >> inStack [
	^ inStack
]

{ #category : #setting }
MalTarjanNode >> inStack: anObject [
	inStack := anObject
]

{ #category : #initialization }
MalTarjanNode >> initialize [
	super initialize.
	nextNodes := Set new.
	index := -1.
	inStack := false
]

{ #category : #testing }
MalTarjanNode >> isInCycle [
	^ self cycleNodes isNotEmpty
]

{ #category : #testing }
MalTarjanNode >> isTarjanUndefined [
	^ index = -1
]

{ #category : #accessing }
MalTarjanNode >> label [
	^ 'T '
]

{ #category : #setting }
MalTarjanNode >> popped [
	self inStack: false
]

{ #category : #accessing }
MalTarjanNode >> tarjanIndex [
	^ index
]

{ #category : #setting }
MalTarjanNode >> tarjanIndex: anInteger [
	index := anInteger
]

{ #category : #accessing }
MalTarjanNode >> tarjanLowlink [
	^ lowlink
]

{ #category : #setting }
MalTarjanNode >> tarjanLowlink: anInteger [
	lowlink := anInteger
]
