Extension { #name : #StringMorph }

{ #category : #'*Athens-Morphic' }
StringMorph >> drawOnAthensCanvas: canvas [
	| bnd |
"	bnd := self bounds.
	gap := self layoutInset.
"	bnd := self bounds topLeft + self layoutInset.

	font := self fontToUse.

	canvas pathTransform restoreAfter: [
		canvas pathTransform translateBy: bnd.
		canvas pathTransform translateX: 0 Y: font getPreciseAscent.

		canvas
			setPaint: color;
			setFont: self fontToUse;
			drawString: contents.
	]
]
