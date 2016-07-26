private
fromDashedBorderStyle: border on: anAthensCanvas
	| allDashLength offset width |
	width := border width.
	allDashLength := border dashLengths detectSum: #yourself.
	offset := 0.
	strokePaints := border dashColors
		collectWithIndex: [ :c :i | 
			| stroke dashLength |
			dashLength := border dashLengths at: i.
			stroke := c asStrokePaintOn: anAthensCanvas.
			stroke width: width.
			stroke capButt.
			stroke
				dashes:
					{(dashLength * width).
					((allDashLength - dashLength) * width)}
				offset: (allDashLength - offset) * width.
			offset := offset + dashLength.
			stroke ].
	self withInset: false