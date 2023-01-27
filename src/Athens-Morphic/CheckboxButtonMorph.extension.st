Extension { #name : #CheckboxButtonMorph }

{ #category : #'*Athens-Morphic' }
CheckboxButtonMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw the image for the current state."

	| img |
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas drawShape: self bounds.
	anAthensCanvas setPaint: self borderStyle.
	anAthensCanvas drawShape: self bounds.
	img := self imageToUse.
	img
		ifNotNil: [
			anAthensCanvas setPaint: img.
			anAthensCanvas drawShape: (self innerBounds center - (img extent // 2) extent: img extent) ].
	((self state == #pressed or: [ self state == #repressed ]) and: [ image isNil ])
		ifTrue: [
			anAthensCanvas setPaint: (self paneColor alpha: 0.3).
			anAthensCanvas drawShape: self innerBounds ].
	(self enabled not and: [ self theme fadeCheckboxWhenDisabled ])
		ifTrue: [
			anAthensCanvas setPaint: (self paneColor alpha: 0.4).
			anAthensCanvas drawShape: self innerBounds ]
]
