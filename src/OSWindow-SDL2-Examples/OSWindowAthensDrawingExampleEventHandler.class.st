"
i a m the event handler linked to SDL2AthensDrawingExample
"
Class {
	#name : #OSWindowAthensDrawingExampleEventHandler,
	#superclass : #OSWindowAthensExampleEventHandler,
	#category : #OSWindow-SDL2-Examples-Basic
}

{ #category : #visiting }
OSWindowAthensDrawingExampleEventHandler >> visitMouseButtonPressEvent: anEvent [
	anEvent button = 1
		ifTrue: [ ^ athensExample startStrokeAt: anEvent position ].
	athensExample clearStrokes
]

{ #category : #visiting }
OSWindowAthensDrawingExampleEventHandler >> visitMouseButtonReleaseEvent: anEvent [
	athensExample endStroke
]

{ #category : #visiting }
OSWindowAthensDrawingExampleEventHandler >> visitMouseMoveEvent: anEvent [
	athensExample moveAt: anEvent position
]
