Extension { #name : 'BitmapFillStyle' }

{ #category : '*Athens-Morphic' }
BitmapFillStyle >> asAthensPaintOn: anAthensCanvas [
	| paint |
	paint := (anAthensCanvas surface createFormPaint: form)
		origin: origin;
		direction: self direction; yourself.
	self isTiled
		ifTrue: [ paint repeat ].
	^ paint
]
