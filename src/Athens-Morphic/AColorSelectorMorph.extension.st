Extension { #name : #AColorSelectorMorph }

{ #category : #'*Athens-Morphic' }
AColorSelectorMorph >> drawOnAthensCanvas: anAthensCanvas [
	anAthensCanvas setPaint: (InfiniteForm with: self hatchForm).
	anAthensCanvas drawShape: self innerBounds.
	super drawOnAthensCanvas: anAthensCanvas
]
