"
A modified canvas which clips all drawing commands.
"
Class {
	#name : #ClippingCanvas,
	#superclass : #PluggableCanvas,
	#instVars : [
		'canvas',
		'clipRect'
	],
	#category : #Graphics-Canvas
}

{ #category : #'instance creation' }
ClippingCanvas class >> canvas: aCanvas  clipRect: aRectangle [
	^self new canvas: aCanvas  clipRect: aRectangle
]

{ #category : #private }
ClippingCanvas >> apply: aBlock [
	"apply the given block to the inner canvas with clipRect as the clipping rectangle"
	canvas clipBy: clipRect during: aBlock
]

{ #category : #initialization }
ClippingCanvas >> canvas: aCanvas  clipRect: aRectangle [
	canvas := aCanvas.
	clipRect := aRectangle.
]

{ #category : #accessing }
ClippingCanvas >> clipRect [
	^clipRect
]

{ #category : #accessing }
ClippingCanvas >> contentsOfArea: aRectangle into: aForm [
	self flag: #hack.    "ignore the clipping specification for this command.  This is purely so that CachingCanvas will work properly when clipped.  There *has* to be a clean way to do this...."
	
	^canvas contentsOfArea: aRectangle into: aForm
]

{ #category : #accessing }
ClippingCanvas >> form [
	^canvas form
]

{ #category : #testing }
ClippingCanvas >> isBalloonCanvas [
	^canvas isBalloonCanvas
]

{ #category : #testing }
ClippingCanvas >> isShadowDrawing [
	^canvas isShadowDrawing
]

{ #category : #accessing }
ClippingCanvas >> shadowColor [
	^canvas shadowColor
]
