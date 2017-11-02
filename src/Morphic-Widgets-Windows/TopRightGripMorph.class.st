"
I am the handle in the right top of windows used for resizing them.
"
Class {
	#name : #TopRightGripMorph,
	#superclass : #CornerGripMorph,
	#category : 'Morphic-Widgets-Windows-Resizing'
}

{ #category : #'target resize' }
TopRightGripMorph >> apply: delta [ 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + (0@delta y) corner: oldBounds corner + (delta x @ 0))
]

{ #category : #testing }
TopRightGripMorph >> containsPoint: aPoint [
	"Answer true only if on edges."
	
	|w|
	^(super containsPoint: aPoint) and: [
		w := SystemWindow borderWidth.
		((self bounds translateBy: w negated@w)
			containsPoint: aPoint) not]
]

{ #category : #drawing }
TopRightGripMorph >> drawOn: aCanvas [
	"Draw the grip on the given canvas."

	| dotBounds alphaCanvas windowBorderWidth dotBounds2 |
	
	self shouldDraw ifFalse: [^self].

	windowBorderWidth := SystemWindow borderWidth.
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.	
	"alphaCanvas
		frameRectangle: bounds color: Color blue."

	dotBounds := self bounds.
	dotBounds2 := dotBounds left: (dotBounds right - windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 top + windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.
	
	dotBounds2 := dotBounds right: (dotBounds right - windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 top + windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 left: (dotBounds2 left + 7).
	dotBounds2 := dotBounds2 right: (dotBounds2 right - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor.

	dotBounds2 := dotBounds left: (dotBounds right - windowBorderWidth).
	dotBounds2 := dotBounds2 top: (dotBounds2 top + windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 top: (dotBounds2 top + 7).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 bottom - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor
]

{ #category : #accessing }
TopRightGripMorph >> gripLayoutFrame [
	^ (1 @ 0 corner: 1 @ 0) asLayoutFrame 
			topLeftOffset: self defaultWidth negated @ -27
]

{ #category : #accessing }
TopRightGripMorph >> ptName [
	^#topRight
]

{ #category : #accessing }
TopRightGripMorph >> resizeCursor [

	^ Cursor resizeForEdge: #topRight
]
