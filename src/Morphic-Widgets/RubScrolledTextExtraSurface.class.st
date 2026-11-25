Class {
	#name : 'RubScrolledTextExtraSurface',
	#superclass : 'RubScrolledTextExtra',
	#category : 'Morphic-Widgets-Editing-Widgets',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Widgets'
}

{ #category : 'accessing' }
RubScrolledTextExtraSurface >> computedHeightFrom: aRectangle [
	^ aRectangle height min: self scrollBounds height
]

{ #category : 'accessing' }
RubScrolledTextExtraSurface >> computedWidthFrom: aRectangle [
	^  aRectangle width min: self scrollBounds width
]

{ #category : 'testing' }
RubScrolledTextExtraSurface >> isOver [
	^ true
]

{ #category : 'geometry' }
RubScrolledTextExtraSurface >> manageLayoutInBounds: aRectangle [
	self position: (self computedPositionFrom: aRectangle).
	self extent: (self computedExtentFrom: aRectangle).
	self changed.
	^ aRectangle
]
