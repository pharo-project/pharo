Extension { #name : #CompositeFillStyle }

{ #category : #'*Athens-Morphic' }
CompositeFillStyle >> asAthensPaintOn: aCanvas [
	^ AthensCompositePaint new fromFillStyles: fillStyles on: aCanvas
]
