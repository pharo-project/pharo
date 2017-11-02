Extension { #name : #BorderStyle }

{ #category : #'*Athens-Morphic' }
BorderStyle >> asAthensPaintOn: anAthensCanvas [
	| stroke |
	stroke := self color asStrokePaintOn: anAthensCanvas.
	stroke width: self width.
	stroke joinMiter.
	^ stroke
]
