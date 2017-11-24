Class {
	#name : #GTSpotterBreadcrumbSeparatorRenderer,
	#superclass : #GLMBrickRenderer,
	#category : #'GT-Spotter-Renderer'
}

{ #category : #rendering }
GTSpotterBreadcrumbSeparatorRenderer >> render: aBrick on: aCanvas [
	super renderBackgroundColorOf: aBrick on: aCanvas.
	
	self renderArrow: aBrick on: aCanvas
]

{ #category : #rendering }
GTSpotterBreadcrumbSeparatorRenderer >> renderArrow: aBrick on: aCanvas [
	|height width hW hH aX bX cX aY bY cY a b c overdraw delta|
	
	overdraw := 2.
	delta := 1.
	height := aBrick brickBounds height.
	width := aBrick brickBounds width.
	hH := height / 4.
	hW := width / 2.
	
	aX := (((hW - hH) max: 0) min: width) floor - overdraw - delta.
	bX := (((hW + hH) max: 0) min: width) floor - delta.
	cX := aX.
	
	aY := overdraw negated.
	bY := (hH * 2) floor.
	cY := height + overdraw.
	
	a := aBrick brickBounds topLeft + (aX@aY).
	b := aBrick brickBounds topLeft + (bX@bY).
	c := aBrick brickBounds topLeft + (cX@cY).
	
	aCanvas line: a to: b width: aBrick borderWidth first color: aBrick borderColor first.
	aCanvas line: b to: c width: aBrick borderWidth first color: aBrick borderColor first
]
