"
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.
"
Class {
	#name : #BalloonFillData,
	#superclass : #Object,
	#instVars : [
		'index',
		'minX',
		'maxX',
		'yValue',
		'source',
		'destForm'
	],
	#category : #Balloon-Simulation
}

{ #category : #computing }
BalloonFillData >> computeFill [
	(destForm isNil or:[destForm width < self width]) ifTrue:[
		destForm := Form extent: (self width + 10) @ 1 depth: 32.
	].
	source computeFillFrom: minX to: maxX at: yValue in: destForm
]

{ #category : #accessing }
BalloonFillData >> destForm [
	^destForm
]

{ #category : #accessing }
BalloonFillData >> destForm: aForm [
	destForm := aForm
]

{ #category : #accessing }
BalloonFillData >> index [
	^index
]

{ #category : #accessing }
BalloonFillData >> index: anInteger [
	index := anInteger
]

{ #category : #accessing }
BalloonFillData >> maxX [
	^maxX
]

{ #category : #accessing }
BalloonFillData >> maxX: anInteger [
	maxX := anInteger
]

{ #category : #accessing }
BalloonFillData >> minX [
	^minX
]

{ #category : #accessing }
BalloonFillData >> minX: anInteger [
	minX := anInteger
]

{ #category : #accessing }
BalloonFillData >> source [
	^source
]

{ #category : #accessing }
BalloonFillData >> source: anObject [
	source := anObject
]

{ #category : #accessing }
BalloonFillData >> width [
	^maxX - minX
]

{ #category : #accessing }
BalloonFillData >> yValue [
	^yValue
]

{ #category : #accessing }
BalloonFillData >> yValue: anInteger [
	yValue := anInteger
]
