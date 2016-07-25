drawing
drawOnAthensCanvas: anAthensCanvas
	| cwidth col dsh lw cw dc nb bnds gap |
	super drawOnAthensCanvas: anAthensCanvas.
	bnds := self bounds.
	cwidth := self width.
	cw := self tabWidth.
	gap := self offset x \\ cw.
	col := cw + self margins left.
	dsh := self tabLineDashes.
	dc := {(self tabLineColor). (Color transparent)}.
	lw := self tabLineWidth.
	nb := cwidth // cw.
	(anAthensCanvas setStrokePaint: self tabLineColor)
		dashes: self tabLineDashes offset: 0;
		width: 1.
	nb
		timesRepeat: [ 
			| b |
			b := bnds topLeft + (col @ 0) corner: bnds bottomLeft + ((col + lw) @ 0).
			b := b translateBy: (cw - gap - cw) @ 0.
			anAthensCanvas setShape: b.
			anAthensCanvas draw.
			col := col + cw ]