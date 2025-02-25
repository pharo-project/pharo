Extension { #name : 'ThreePhaseButtonMorph' }

{ #category : '*Athens-Morphic' }
ThreePhaseButtonMorph >> drawOnAthensCanvas: anAthensCanvas [
	| paint |
	paint := self offImage.

	state == #pressed
		ifTrue: [ paint := self pressedImage ].
	state == #on
		ifTrue: [ paint := self onImage ].
	paint ifNil: [ ^ self ].

	anAthensCanvas setPaint: paint.
	anAthensCanvas paintTransform
		restoreAfter: [
			anAthensCanvas paintTransform translateBy: self innerBounds origin.
			anAthensCanvas drawShape: self innerBounds ]
]
