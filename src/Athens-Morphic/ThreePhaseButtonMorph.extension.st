Extension { #name : #ThreePhaseButtonMorph }

{ #category : #'*Athens-Morphic' }
ThreePhaseButtonMorph >> drawOnAthensCanvas: anAthensCanvas [
	| paint |
	paint := offImage.

	state == #pressed
		ifTrue: [ paint := pressedImage ].
	state == #on
		ifTrue: [ paint := image ].
	paint ifNil: [ ^ self ].

	anAthensCanvas setPaint: paint.
	anAthensCanvas paintTransform
		restoreAfter: [
			anAthensCanvas paintTransform translateBy: self innerBounds origin.
			anAthensCanvas drawShape: self innerBounds ]
]
