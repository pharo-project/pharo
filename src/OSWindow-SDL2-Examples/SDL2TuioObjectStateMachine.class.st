Class {
	#name : #SDL2TuioObjectStateMachine,
	#superclass : #Object,
	#instVars : [
		'color',
		'position',
		'angle',
		'objectId',
		'objectNumber'
	],
	#category : #OSWindow-SDL2-Examples-Touch
}

{ #category : #accessing }
SDL2TuioObjectStateMachine >> angle [
	^ angle
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> angle: anObject [
	angle := anObject
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> color [
	^color
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> color: anObject [
	color := anObject
]

{ #category : #initialization }
SDL2TuioObjectStateMachine >> initialize [
	position := 0@0.
	color := Color random.
	angle := 0.
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> objectId [
	^ objectId
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> objectId: anObject [
	objectId := anObject
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> objectNumber [
	^ objectNumber
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> objectNumber: anObject [
	objectNumber := anObject
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> position [
	^ position
]

{ #category : #accessing }
SDL2TuioObjectStateMachine >> position: anObject [
	position := anObject
]
