"
Superclass to show how to use OSWindow properly with athens
"
Class {
	#name : #SDL2AthensExample,
	#superclass : #Object,
	#instVars : [
		'renderer',
		'drawingProcess',
		'window'
	],
	#category : #'OSWindow-SDL2-Examples-Examples'
}

{ #category : #'instance creation' }
SDL2AthensExample >> createDrawingProcess [
	^ [ 
	[ 
	(Delay forMilliseconds: 1000 / self maxFPS) wait.
	self redrawAndUpdateSurface ] repeat ] fork
]

{ #category : #factory }
SDL2AthensExample >> createEventHandler [
	"You should override this method with your own event handler"
	^ OSWindowAthensExampleEventHandler new 
		athensExample: self
]

{ #category : #'morphic ui' }
SDL2AthensExample >> defaultExtent [
	^ 500@500
]

{ #category : #'morphic ui' }
SDL2AthensExample >> destroy [
	"This function must be called when the window is closed"
	drawingProcess terminate
]

{ #category : #accessing }
SDL2AthensExample >> initializeFromSurface [
	"Here we put all the things that needs the surface to be initialized."
]

{ #category : #'instance creation' }
SDL2AthensExample >> maxFPS [
	^ 60
]

{ #category : #'instance creation' }
SDL2AthensExample >> open [
	| attrs |
	attrs := OSWindowAttributes new.
	attrs 
		extent: self defaultExtent;
		resizable: false;
		position: 100@100.
		
	window := OSWindow createWithAttributes: attrs.
	
	renderer := window newAthensRenderer.

	self initializeFromSurface.

	window eventHandler: (self createEventHandler).
	
	drawingProcess := self createDrawingProcess
]

{ #category : #drawing }
SDL2AthensExample >> redraw [
	^self subclassResponsibility
]

{ #category : #updating }
SDL2AthensExample >> redrawAndUpdateSurface [
	self redraw.
	self updateSurface
]

{ #category : #updating }
SDL2AthensExample >> updateSurface [
	renderer
		updateAll;
		present
]
