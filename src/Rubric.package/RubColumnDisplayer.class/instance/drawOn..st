drawing
drawOn: aCanvas
	| cwidth col dsh lw cw dc nb bnds gap |
	super drawOn: aCanvas.
	bnds := self bounds.
	cwidth := self width.
	cw := self tabWidth.
	gap := self offset x \\ cw .
	col := cw + self margins left.
	dsh := self tabLineDashes.
	dc := {(self tabLineColor). (Color transparent)}.
	lw := self tabLineWidth.
	nb := cwidth // cw.
	nb timesRepeat: [ 
			| b |
			b :=  (bnds topLeft + (col @ 0) ) corner: (bnds bottomLeft + ((col + lw) @ 0) ).
			b := b translateBy: (((cw - gap) - cw)@ 0).
			aCanvas
				frameRectangle: b
				width: 1
				colors: dc
				dashes: dsh.
			col := col + cw ]