paints
registerRadialGradient: colorRamp center: aCenter radius: r 
	| pixelRamp |
	
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.
	
	currentFill := 
		self primAddGradientFill: pixelRamp
			from: aCenter
			along: (aCenter x + r @ aCenter y)
			normal:  (aCenter x  @ aCenter y - r)
			radial:  true.