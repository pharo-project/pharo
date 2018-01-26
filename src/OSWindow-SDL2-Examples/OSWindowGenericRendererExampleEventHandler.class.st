"
I am the event handler linked to the SDL2JoystickExample
"
Class {
	#name : #OSWindowGenericRendererExampleEventHandler,
	#superclass : #OSWindowEventVisitor,
	#instVars : [
		'example'
	],
	#category : #'OSWindow-SDL2-Examples-Examples'
}

{ #category : #accessing }
OSWindowGenericRendererExampleEventHandler >> example [
	<sampleInstance>
	^ example
]

{ #category : #accessing }
OSWindowGenericRendererExampleEventHandler >> example: anObject [
	<sampleInstance>
	example := anObject
]

{ #category : #visiting }
OSWindowGenericRendererExampleEventHandler >> visitWindowCloseEvent: anEvent [
	example destroy
]

{ #category : #visiting }
OSWindowGenericRendererExampleEventHandler >> visitWindowExposeEvent: anEvent [
	example enqueueRender.
	anEvent suppressDefaultAction.
]

{ #category : #visiting }
OSWindowGenericRendererExampleEventHandler >> visitWindowFrameEvent: anEvent [
	example ifNotNil: [ example update: Time millisecondClockValue *0.001. ].
	example enqueueRender 
]
