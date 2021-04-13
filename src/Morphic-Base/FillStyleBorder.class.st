"
BorderStyle supporting general (potentially composite) fillstyles. 
"
Class {
	#name : #FillStyleBorder,
	#superclass : #SimpleBorder,
	#instVars : [
		'fillStyle'
	],
	#category : #'Morphic-Base-Borders'
}

{ #category : #geometry }
FillStyleBorder >> borderRectsFor: aRectangle [
	"Answer a collection of rectangles to fill.
	Just four here for a rectangular border."

	|rTop rBottom rLeft rRight w|
	w := self width.
	rTop := aRectangle topLeft corner: aRectangle right @ (aRectangle top + w).
	rBottom := aRectangle left @ (aRectangle bottom - w) corner: aRectangle bottomRight.
	rLeft := aRectangle left @ (aRectangle top + w) corner: aRectangle left + w @ (aRectangle bottom - w).
	rRight := aRectangle right - w @ (aRectangle top + w) corner: aRectangle right @ (aRectangle bottom - w).
	^{rTop. rBottom. rLeft. rRight}
]

{ #category : #accessing }
FillStyleBorder >> fillStyle [
	"Answer the value of fillStyle"

	^fillStyle ifNil: [self color]
]

{ #category : #accessing }
FillStyleBorder >> fillStyle: anObject [
	"Set the value of fillStyle"

	fillStyle := anObject.
	anObject ifNotNil: [self baseColor: anObject asColor]
]

{ #category : #drawing }
FillStyleBorder >> frameRectangle: aRectangle on: aCanvas [
	"Fill the border areas with the fill style, clipping for each segment."
	
	(self borderRectsFor: aRectangle) do: [:r |
		aCanvas
			fillRectangle: r
			fillStyle: self fillStyle]
]

{ #category : #testing }
FillStyleBorder >> hasFillStyle [
	"Answer true."

	^true
]
