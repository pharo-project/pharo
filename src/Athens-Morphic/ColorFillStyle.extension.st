Extension { #name : #ColorFillStyle }

{ #category : #'*Athens-Morphic' }
ColorFillStyle >> asAthensPaintOn: anAthensCanvas [
	^ (anAthensCanvas surface createSolidColorPaint: color)
		origin: origin;
		extent: extent;
		yourself
]
