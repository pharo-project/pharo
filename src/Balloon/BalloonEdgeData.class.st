"
BalloonEdgeData defines an entry in the internal edge table of the Balloon engine.

Instance Variables:
	index	<Integer>	The index into the external objects array of the associated graphics engine
	xValue	<Integer>	The computed x-value of the requested operation
	yValue	<Integer>	The y-value for the requested operation
	height	<Integer>	The (remaining) height of the edge
	source	<Object>		The object from the external objects array
"
Class {
	#name : #BalloonEdgeData,
	#superclass : #Object,
	#instVars : [
		'index',
		'xValue',
		'yValue',
		'zValue',
		'lines',
		'source'
	],
	#category : 'Balloon-Simulation'
}

{ #category : #accessing }
BalloonEdgeData >> index [
	^index
]

{ #category : #accessing }
BalloonEdgeData >> index: anInteger [
	index := anInteger
]

{ #category : #accessing }
BalloonEdgeData >> lines [
	^lines
]

{ #category : #accessing }
BalloonEdgeData >> lines: anInteger [
	^lines := anInteger
]

{ #category : #accessing }
BalloonEdgeData >> source [
	^source
]

{ #category : #accessing }
BalloonEdgeData >> source: anObject [
	source := anObject
]

{ #category : #computing }
BalloonEdgeData >> stepToFirstScanLine [
	source stepToFirstScanLineAt: yValue in: self
]

{ #category : #computing }
BalloonEdgeData >> stepToNextScanLine [
	source stepToNextScanLineAt: yValue in: self
]

{ #category : #accessing }
BalloonEdgeData >> xValue [
	^xValue
]

{ #category : #accessing }
BalloonEdgeData >> xValue: anInteger [
	xValue := anInteger
]

{ #category : #accessing }
BalloonEdgeData >> yValue [
	^yValue
]

{ #category : #accessing }
BalloonEdgeData >> yValue: anInteger [
	yValue := anInteger
]

{ #category : #accessing }
BalloonEdgeData >> zValue [
	^zValue
]

{ #category : #accessing }
BalloonEdgeData >> zValue: anInteger [
	zValue := anInteger
]
