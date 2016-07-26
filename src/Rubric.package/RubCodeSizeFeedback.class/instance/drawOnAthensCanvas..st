drawing
drawOnAthensCanvas: anAthensCanvas
	| font s bnds |
	super drawOnAthensCanvas: anAthensCanvas.
	color = self normalColor
		ifTrue: [ ^ self ].
	font := self paragraph textStyle defaultFont.
	s := self textSize asString.
	bnds := self bounds withTop: self bounds bottom - font height.
	anAthensCanvas morphicDrawString: s in: bnds font: font color: self stringColor