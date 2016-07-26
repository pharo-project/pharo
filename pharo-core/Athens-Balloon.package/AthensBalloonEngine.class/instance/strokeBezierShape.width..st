private
strokeBezierShape: contours width: aWidth

	contours do:[ :points |
		self primAddBezierShape: points
			segments: (points size // 3)
			fill: 0
			lineWidth: aWidth
			lineFill: currentFill.
			
		"Note: To avoid premature flushing of the pipeline we need to reset the flush bit within the engine."
		self primFlushNeeded: false.
	].

	self copyBits. 
