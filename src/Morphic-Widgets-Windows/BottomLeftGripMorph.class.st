"
I am the handle in the left bottom of windows used for resizing them.
"
Class {
	#name : #BottomLeftGripMorph,
	#superclass : #CornerGripMorph,
	#category : #'Morphic-Widgets-Windows-Resizing'
}

{ #category : #'target resize' }
BottomLeftGripMorph >> apply: delta [ 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + (delta x @ 0) corner: oldBounds corner + (0 @ delta y))
]

{ #category : #testing }
BottomLeftGripMorph >> containsPoint: aPoint [
	"Answer true only if on edges."
	
	|w|
	^(super containsPoint: aPoint) and: [
		w := SystemWindow borderWidth.
		((self bounds translateBy: w@w negated)
			containsPoint: aPoint) not]
]

{ #category : #drawing }
BottomLeftGripMorph >> drawOn: aCanvas [
	"Draw the grip on the given canvas."

	| dotBounds alphaCanvas windowBorderWidth dotBounds2 |
	
	self shouldDraw ifFalse: [^self].

	windowBorderWidth := SystemWindow borderWidth.
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.	
	"alphaCanvas
		frameRectangle: bounds color: Color blue."

	dotBounds := self bounds.
	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 top: (dotBounds2 bottom - windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.
	
	dotBounds2 := dotBounds left: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 top: (dotBounds2 bottom - windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 left: (dotBounds2 left + 7).
	dotBounds2 := dotBounds2 right: (dotBounds2 right - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor.

	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 bottom - windowBorderWidth).
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
BottomLeftGripMorph >> gripLayoutFrame [
	^  (0 @ 1 corner: 0 @ 1) asLayoutFrame 
			topOffset: self defaultHeight negated;
			rightOffset: self defaultWidth
]

{ #category : #accessing }
BottomLeftGripMorph >> ptName [
	^#bottomLeft
]

{ #category : #accessing }
BottomLeftGripMorph >> resizeCursor [

	^ Cursor resizeForEdge: #bottomLeft
]
