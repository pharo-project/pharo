"
I am the event handler linked to the SDL2AthensExample
"
Class {
	#name : #OSWindowAthensExampleEventHandler,
	#superclass : #OSWindowEventVisitor,
	#instVars : [
		'athensExample'
	],
	#category : #OSWindow-SDL2-Examples-Examples
}

{ #category : #accessing }
OSWindowAthensExampleEventHandler >> athensExample [
	^ athensExample
]

{ #category : #accessing }
OSWindowAthensExampleEventHandler >> athensExample: anObject [
	athensExample := anObject
]

{ #category : #visiting }
OSWindowAthensExampleEventHandler >> visitWindowCloseEvent: anEvent [
	athensExample destroy
]
