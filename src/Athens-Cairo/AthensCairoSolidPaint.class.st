"
i represent solid color paint in cairo backend
"
Class {
	#name : #AthensCairoSolidPaint,
	#superclass : #AthensPaint,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'r',
		'g',
		'b',
		'a',
		'origin',
		'extent'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #drawing }
AthensCairoSolidPaint >> athensFillPath: path on: aCanvas [

	aCanvas
		setSourceR: r g: g b: b a: a;
		newPath;
		loadPath: path;
		fill
]

{ #category : #drawing }
AthensCairoSolidPaint >> athensFillRectangle: aRect on: aCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "
	|o e toFillRect|
	o := origin ifNil:[aRect origin] ifNotNil:[origin].
	e := extent ifNil:[aRect extent] ifNotNil:[extent].
	toFillRect := o extent:e.

	aCanvas
		setSourceR: r g: g b: b a: a;
		newPath;
		rectangleX: toFillRect left y: toFillRect top width: toFillRect width height: toFillRect height;
		fill
]

{ #category : #accessing }
AthensCairoSolidPaint >> color: aColor [
	r := aColor red.
	g := aColor green.
	b := aColor blue.
	a := aColor alpha
]

{ #category : #accessing }
AthensCairoSolidPaint >> extent: anExtent [
	extent := anExtent
]

{ #category : #drawing }
AthensCairoSolidPaint >> loadOnCairoCanvas: aCanvas [
	aCanvas
		setSourceR: r g: g b: b a: a
]

{ #category : #accessing }
AthensCairoSolidPaint >> origin: aPoint [
	origin := aPoint
]
