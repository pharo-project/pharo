"
I am an abstract interface for a scene that is drawn with a generic renderer. I provide methods that are called by the windowing system when immediate redrawing is required due to a window exposure state change.
"
Class {
	#name : #OSWindowGenericRendererScene,
	#superclass : #Object,
	#category : #'OSWindow-Core-Renderer'
}

{ #category : #rendering }
OSWindowGenericRendererScene >> renderAllWith: renderer [
	"This is the place where the whole scene must be redrawn with the renderer."
]

{ #category : #rendering }
OSWindowGenericRendererScene >> renderRectangle: rectangle with: aRenderer [
	"This is the place where only a single rectangle of scene actually needs to be drawn. By default, redraw the whole scene."
	^ self renderAllWith: aRenderer
]

{ #category : #rendering }
OSWindowGenericRendererScene >> renderRectangles: aCollectionOfRectangles with: aRenderer [
	"This is the place where a collection of rectangles in the scene actually needs to be drawn.
	By default, redraw the union of the rectangles."
	^ self renderRectangle: (Rectangle merging: aCollectionOfRectangles) with: aRenderer
]
