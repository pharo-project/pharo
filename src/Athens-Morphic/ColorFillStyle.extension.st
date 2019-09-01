Extension { #name : #ColorFillStyle }

{ #category : #'*Athens-Morphic' }
ColorFillStyle >> = anObject [

	^ super = anObject
		and: [ origin = anObject origin
			and: [ extent = anObject extent ] ]
]

{ #category : #'*Athens-Morphic' }
ColorFillStyle >> asAthensPaintOn: anAthensCanvas [
	^ (anAthensCanvas surface createSolidColorPaint: color)
		origin: origin;
		extent: extent;
		yourself
]
