drawing
drawOnAthensCanvas: anAthensCanvas
	| bnds adorncolor size fillStyle vertices polygonPath |
	super drawOnAthensCanvas: anAthensCanvas.
	self paragraphProvider hasUnacceptedEdits
		ifFalse: [ ^ self ].
	bnds := self bounds.
	adorncolor := self paragraphProvider adornmentColor.
	size := self width.
	fillStyle := (GradientFillStyle
		ramp:
			{(0.0 -> (Color white alpha: 0.3)).
			(0.8 -> adorncolor).
			(1.0 -> adorncolor)})
		origin: bnds topRight - (size @ 0);
		direction: size @ size negated // 4;
		radial: false.
	vertices := {(bnds topRight).
	(bnds topRight + (0 @ size)).
	(bnds topRight - (size @ 0))}.
	polygonPath := anAthensCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: vertices first.
			vertices allButFirstDo: [ :pt | builder lineTo: pt ].
			builder close ].
	anAthensCanvas setPaint: fillStyle.
	anAthensCanvas setShape: polygonPath.
	anAthensCanvas draw