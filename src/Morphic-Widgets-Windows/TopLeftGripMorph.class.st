"
I am the handle in the left top of windows used for resizing them.
"
Class {
	#name : #TopLeftGripMorph,
	#superclass : #CornerGripMorph,
	#category : #Morphic-Widgets-Windows-Resizing
}

{ #category : #'target resize' }
TopLeftGripMorph >> apply: delta [ 
	| oldBounds |
	oldBounds := target bounds.
	target
		bounds: (oldBounds origin + delta corner: oldBounds corner)
]

{ #category : #testing }
TopLeftGripMorph >> containsPoint: aPoint [
	"Answer true only if on edges."
	
	|w|
	^(super containsPoint: aPoint) and: [
		w := SystemWindow borderWidth.
		((self bounds translateBy: w@w)
			containsPoint: aPoint) not]
]

{ #category : #drawing }
TopLeftGripMorph >> drawOn: aCanvas [
	"Draw the grip on the given canvas."

	| dotBounds alphaCanvas windowBorderWidth dotBounds2 |
	
	self shouldDraw ifFalse: [^self].

	windowBorderWidth := SystemWindow borderWidth.
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.	
	"alphaCanvas
		frameRectangle: bounds color: Color blue."

	dotBounds := self bounds.
	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 top + windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.
	
	dotBounds2 := dotBounds left: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 top + windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 left: (dotBounds2 left + 7).
	dotBounds2 := dotBounds2 right: (dotBounds2 right - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor.

	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
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
TopLeftGripMorph >> gripLayoutFrame [
	^ (0 @ 0 corner: 0 @ 0) asLayoutFrame 
			topOffset: -27;
		 	rightOffset: self defaultWidth
]

{ #category : #'target resize' }
TopLeftGripMorph >> ptName [
	^#topLeft
]

{ #category : #accessing }
TopLeftGripMorph >> resizeCursor [

	^ Cursor resizeForEdge: #topLeft
]
