"
My instances are used for updating the OSWindow contents by rendering on it.
There is no way to obtain a valid instance of window renderer other than asking a valid instance of OSWindow for window renderer instance.
See newFormRenderer: / newOpenGLRenderer factory methods.
"
Class {
	#name : #OSWindowRenderer,
	#superclass : #Object,
	#instVars : [
		'window',
		'lastExposeTime'
	],
	#category : #OSWindow-Core
}

{ #category : #deleting }
OSWindowRenderer >> destroy [
	"Called when the window associated with the renderer is destroyed"
	window := nil.
]

{ #category : #'updating screen' }
OSWindowRenderer >> exposed [
	self present
]

{ #category : #'updating screen' }
OSWindowRenderer >> exposed: exposeTime [
	(lastExposeTime notNil and: [ exposeTime <= lastExposeTime]) ifTrue: [ ^ self ].
	lastExposeTime := exposeTime.
	self exposed
]

{ #category : #initialization }
OSWindowRenderer >> initialize [
	lastExposeTime := 0
]

{ #category : #'*OSWindow-Core-Extras' }
OSWindowRenderer >> onRenderThread: aBlock [
	OSWindowRenderThread enqueueOperation: aBlock
]

{ #category : #'*OSWindow-Core-Extras' }
OSWindowRenderer >> onRenderThreadBlocking: aBlock [
	OSWindowRenderThread enqueueBlockingOperation: aBlock
]

{ #category : #'updating screen' }
OSWindowRenderer >> present [
	"This should present the changes to the next frame."
	self updateAll
]

{ #category : #'updating screen' }
OSWindowRenderer >> resized [
	self updateAll
]

{ #category : #'updating screen' }
OSWindowRenderer >> updateAll [
		"do something to update whole contents of the window"
		
		self subclassResponsibility 
]

{ #category : #'updating screen' }
OSWindowRenderer >> updateRectangle: rectangle [
		"do something to update given portion of the window"
		
		self subclassResponsibility 
]

{ #category : #'updating screen' }
OSWindowRenderer >> updateRectangles: aCollectionOfRectangles [
	aCollectionOfRectangles do: [ :rectangle | self updateRectangle: rectangle ]
]

{ #category : #accessing }
OSWindowRenderer >> window [
	
	^ window
]

{ #category : #accessing }
OSWindowRenderer >> window: anObject [
	
	window := anObject
]
