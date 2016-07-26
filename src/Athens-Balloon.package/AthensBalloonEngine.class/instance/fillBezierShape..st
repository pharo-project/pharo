drawing
fillBezierShape: contours

	contours do:[ :points |
		self primAddBezierShape: points
			segments: (points size // 3)
			fill: currentFill
			lineWidth: 0
			lineFill: 0.
			
		"Note: To avoid premature flushing of the pipeline we need to reset the flush bit within the engine."
		self primFlushNeeded: false.
	].

	self copyBits.