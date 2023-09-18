"
i represent texture/image/form/surface paints in cairo backend.
"
Class {
	#name : #AthensCairoPatternSurfacePaint,
	#superclass : #AthensCairoPatternPaint,
	#instVars : [
		'surface',
		'origin',
		'extent'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #'instance creation' }
AthensCairoPatternSurfacePaint class >> createForSurface: anAthensCairoSurface [
	|pat|
	pat := self primCreateForSurface: anAthensCairoSurface .
	pat initializeFor: anAthensCairoSurface  .
	^ pat
]

{ #category : #primitives }
AthensCairoPatternSurfacePaint class >> primCreateForSurface: anAthensCairoSurface [

"	pattern = cairo_pattern_create_for_surface (image);"

	^ self ffiCall: #(AthensCairoPatternSurfacePaint cairo_pattern_create_for_surface(
		cairo_surface_t  anAthensCairoSurface ) )
]

{ #category : #drawing }
AthensCairoPatternSurfacePaint >> athensFillRectangle: aRectangle on: aCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	extent
		ifNotNil: [ self setRect: (origin extent: extent) forFillOn: aCanvas ]
		ifNil: [ self setRect: aRectangle forFillOn: aCanvas ].

	self setMatrix: aCanvas paintTransform inverted.
	self loadOnCairoCanvas: aCanvas.
	aCanvas fill
]

{ #category : #accessing }
AthensCairoPatternSurfacePaint >> extent [
	^ extent ifNil:[surface extent]
]

{ #category : #accessing }
AthensCairoPatternSurfacePaint >> extent: aPatternExtent [
	extent:= aPatternExtent
]

{ #category : #initialization }
AthensCairoPatternSurfacePaint >> initializeFor: anAthensCairoSurface [
	self initialize.
	surface := anAthensCairoSurface
]

{ #category : #accessing }
AthensCairoPatternSurfacePaint >> origin: aPoint [
	origin := aPoint
]

{ #category : #accessing }
AthensCairoPatternSurfacePaint >> setMatrix: aMatrix [

	aMatrix restoreAfter: [
		origin ifNotNil: [ aMatrix translateBy: origin negated ].
		super setMatrix: aMatrix.
	]
]

{ #category : #private }
AthensCairoPatternSurfacePaint >> setRect: aRectangle forFillOn: aCanvas [
	aCanvas
		rectangleX: aRectangle left
		y: aRectangle top
		width: aRectangle width
		height: aRectangle height
]
