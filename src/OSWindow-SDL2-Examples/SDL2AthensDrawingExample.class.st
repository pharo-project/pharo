"
I am a basic example to show how we can handle an event and draw something with this event.

Left mouse button = draw lines 
Other mouse buttons = clear sketch

To launch the example:
self new open
"
Class {
	#name : #SDL2AthensDrawingExample,
	#superclass : #SDL2AthensExample,
	#instVars : [
		'strokes',
		'currentStroke'
	],
	#category : #'OSWindow-SDL2-Examples-Basic'
}

{ #category : #'managing strokes' }
SDL2AthensDrawingExample >> clearStrokes [
	strokes removeAll
]

{ #category : #drawing }
SDL2AthensDrawingExample >> clearSurface [
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas
				setPaint: Color white;
				drawShape: (0 @ 0 extent: window extent) ]
]

{ #category : #factory }
SDL2AthensDrawingExample >> createEventHandler [
	^ OSWindowAthensDrawingExampleEventHandler new 
			athensExample: self
]

{ #category : #'managing strokes' }
SDL2AthensDrawingExample >> endStroke [
	currentStroke := OrderedCollection new.
]

{ #category : #initialization }
SDL2AthensDrawingExample >> initialize [
	super initialize.
	currentStroke := OrderedCollection new.
	strokes := OrderedCollection new
]

{ #category : #'managing strokes' }
SDL2AthensDrawingExample >> moveAt: aPoint [
	currentStroke add: aPoint
]

{ #category : #updating }
SDL2AthensDrawingExample >> redraw [
	self clearSurface.
	strokes
		do:
			[ :stroke | 
			| path |
			path := renderer athensSurface
				createPath: [ :builder | 
					builder
						absolute;
						moveTo: stroke first.
					stroke do: [ :point | builder lineTo: point ] ].
			renderer drawDuring: [ :canvas | 
					canvas
						setStrokePaint: Color black;
						setShape: path;
						draw ] ]
]

{ #category : #'managing strokes' }
SDL2AthensDrawingExample >> startStrokeAt: aPoint [
	currentStroke := OrderedCollection with: aPoint.
	strokes add: currentStroke
]
