Extension { #name : #DashedBorder }

{ #category : #'*Athens-Morphic' }
DashedBorder >> asAthensPaintOn: anAthensCanvas [
	^ AthensCompositeStrokePaint new fromDashedBorderStyle: self on: anAthensCanvas
]
