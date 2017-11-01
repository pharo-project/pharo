Extension { #name : #CompositeBorder }

{ #category : #'*Athens-Morphic' }
CompositeBorder >> asAthensPaintOn: anAthensCanvas [
	^ AthensCompositeStrokePaint new fromBorderStyles: borders on: anAthensCanvas
]
