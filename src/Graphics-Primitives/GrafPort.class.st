"
Provides extended Graphic support like translucency
"
Class {
	#name : #GrafPort,
	#superclass : #BitBlt,
	#instVars : [
		'alpha',
		'fillPattern',
		'lastFont',
		'lastFontForegroundColor',
		'lastFontBackgroundColor'
	],
	#category : #'Graphics-Primitives'
}

{ #category : #accessing }
GrafPort >> alphaBits: a [
	alpha := a
]

{ #category : #copying }
GrafPort >> clippedBy: aRectangle [
	^ self copy clipBy: aRectangle
]

{ #category : #accessing }
GrafPort >> contentsOfArea: aRectangle into: aForm [
	destForm 
		displayOn: aForm 
		at:  aRectangle origin
		clippingBox: (0@0 extent: aRectangle extent).
	^aForm
]

{ #category : #copying }
GrafPort >> copyBits [
	"Override copybits to do translucency if desired"

	(combinationRule between: 30 and: 31)
		ifTrue: [ self copyBitsTranslucent: (alpha ifNil: [ 255 ]) ]
		ifFalse: [ super copyBits ]
]

{ #category : #'drawing support' }
GrafPort >> fillOval: rect [
	| centerX centerY nextY yBias xBias outer nextOuterX |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	nextY := rect height // 2.
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := rect width.
	destX := rect left.
	self copyBits.

]

{ #category : #accessing }
GrafPort >> fillPattern: anObject [
	fillPattern := anObject.
	self fillColor: anObject.
]

{ #category : #'drawing support' }
GrafPort >> fillRect: rect offset: aPoint [
	"The offset is really just for stupid InfiniteForms."
	| fc |
	fillPattern class == InfiniteForm ifTrue:[
		fc := halftoneForm.
		self fillColor: nil.
		fillPattern displayOnPort: ((self clippedBy: rect) colorMap: nil) at: aPoint.
		halftoneForm := fc.
		^self].

	destX := rect left.
	destY := rect top.
	sourceX := 0.
	sourceY := 0.
	width := rect width.
	height := rect height.
	self copyBits.
]

{ #category : #'drawing support' }
GrafPort >> frameOval: rect borderWidth: borderWidth [
	| centerX centerY nextY yBias xBias wp outer inner nextOuterX nextInnerX |
	rect area <= 0 ifTrue: [^ self].
	height := 1.
	wp := borderWidth asPoint.
	yBias := rect height odd ifTrue: [0] ifFalse: [-1].
	xBias := rect width odd ifTrue: [1] ifFalse: [0].
	centerX := rect center x.
	centerY := rect center y.
	outer := EllipseMidpointTracer new on: rect.
	inner := EllipseMidpointTracer new on: (rect insetBy: wp).
	nextY := rect height // 2.
	1 to: (wp y min: nextY) do:[:i|
		nextOuterX := outer stepInY.
		width := (nextOuterX bitShift: 1) + xBias.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		self copyBits.
		destY := centerY + nextY + yBias.
		self copyBits.
		nextY := nextY - 1.
	].
	[nextY > 0] whileTrue:[
		nextOuterX := outer stepInY.
		nextInnerX := inner stepInY.
		destX := centerX - nextOuterX.
		destY := centerY - nextY.
		width := nextOuterX - nextInnerX.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		destX := centerX - nextOuterX.
		destY := centerY + nextY + yBias.
		self copyBits.
		destX := centerX + nextInnerX + xBias.
		self copyBits.
		nextY := nextY - 1.
	].
	destY := centerY.
	height := 1 + yBias.
	width := wp x.
	destX := rect left.
	self copyBits.
	destX := rect right - wp x.
	self copyBits.

]

{ #category : #'drawing support' }
GrafPort >> frameRect: rect borderWidth: borderWidth [
	sourceX := 0.
	sourceY := 0.
	(rect areasOutside: (rect insetBy: borderWidth)) do:
		[:edgeStrip | self destRect: edgeStrip; copyBits].

]

{ #category : #'drawing support' }
GrafPort >> frameRectBottom: rect height: h [

	destX := rect left + 1.
	destY := rect bottom - 1.
	width := rect width - 2.
	height := 1.
	1 to: h do: [:i |
		self copyBits.
		destX := destX + 1.
		destY := destY - 1.
		width := width - 2].

]

{ #category : #'drawing support' }
GrafPort >> frameRectRight: rect width: w [

	width := 1.
	height := rect height - 1.
	destX := rect right - 1.
	destY := rect top + 1.
	1 to: w do: [:i |
		self copyBits.
		destX := destX - 1.
		destY := destY + 1.
		height := height - 2].

]

{ #category : #'drawing support' }
GrafPort >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm := aForm.
	combinationRule := rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBits
]

{ #category : #'drawing support' }
GrafPort >> image: aForm at: aPoint sourceRect: sourceRect rule: rule alpha: sourceAlpha [
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm := aForm.
	combinationRule := rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBitsTranslucent: (alpha := (sourceAlpha * 255) truncated min: 255 max: 0).
]

{ #category : #accessing }
GrafPort >> lastFontForegroundColor [
	^lastFontForegroundColor
]

{ #category : #'drawing support' }
GrafPort >> stencil: stencilForm at: aPoint sourceRect: aRect [
	"Paint using aColor wherever stencilForm has non-zero pixels"
	self sourceForm: stencilForm;
		destOrigin: aPoint;
		sourceRect: aRect.
	self copyBits
]
