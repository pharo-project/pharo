"
I am an abstract Athens renderer. 
"
Class {
	#name : #OSWindowAthensRenderer,
	#superclass : #OSWindowRenderer,
	#instVars : [
		'athensSurface',
		'session'
	],
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #accessing }
OSWindowAthensRenderer >> athensSurface [
	^ athensSurface
]

{ #category : #'session management' }
OSWindowAthensRenderer >> checkSession [
	session == Smalltalk session 
		ifFalse: [ self error: 'Cannot use this renderer (created in a previous session or in a closed window)' ].
]

{ #category : #deleting }
OSWindowAthensRenderer >> destroy [
	
	super destroy.
	athensSurface := nil.
	session := nil.
]

{ #category : #drawing }
OSWindowAthensRenderer >> drawDuring: aBlock [
	self prepareForDrawing.
	self athensSurface drawDuring: aBlock
]

{ #category : #initialization }
OSWindowAthensRenderer >> initialize [
	super initialize.
	session := Smalltalk session
]

{ #category : #accessing }
OSWindowAthensRenderer >> pixelExtent [
	"The extent of the window may not be in pixels, so we have to ask to the actual
	drawing surface for its size in pixel. This is specially true under OS X with HiDPI suppport."
	self checkSession.
	^ self athensSurface extent
]

{ #category : #drawing }
OSWindowAthensRenderer >> prepareForDrawing [
]

{ #category : #accessing }
OSWindowAthensRenderer >> surface [
	^ self athensSurface
]
