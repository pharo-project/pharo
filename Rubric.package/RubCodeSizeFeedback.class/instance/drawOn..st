drawing
drawOn: aCanvas
	| font s |
	super drawOn: aCanvas.
	color = self normalColor
		ifTrue: [ ^ self ].
	font := self paragraph textStyle defaultFont.
	s := self textSize asString.
	aCanvas
		drawString: s
		in: (self bounds withTop: self bounds bottom - font height)
		font: font
		color: self stringColor