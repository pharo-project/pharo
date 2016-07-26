paints
registerRadialGradient: colorRamp center: aCenter direction: dir normal: n 
	| pixelRamp |
	
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.
	
	currentFill := 
		self primAddGradientFill: pixelRamp
			from: aCenter
			along: dir
			normal:  n
			radial:  true.