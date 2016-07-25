drawing
drawOn: aCanvas
	| bnds adorncolor size fillStyle |
	super drawOn: aCanvas.
	self paragraphProvider isReadOnly ifTrue: [ ^self ].
	self paragraphProvider hasUnacceptedEdits ifFalse: [ ^self ].
	bnds := self bounds.
	adorncolor := self paragraphProvider adornmentColor.
	size := self width.
	fillStyle := (GradientFillStyle ramp: {
			0.0->(Color white alpha: 0.3).
			0.8->adorncolor.
			1.0->adorncolor})
		origin: bnds topRight - (size@0);
		direction: (size @ size negated) // 4;
		radial: false.
	aCanvas
		drawPolygon:  {bnds topRight. bnds topRight + (0@size). bnds topRight - (size@0)}
		fillStyle: fillStyle