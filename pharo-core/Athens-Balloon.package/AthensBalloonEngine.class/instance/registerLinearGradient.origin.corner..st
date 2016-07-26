paints
registerLinearGradient: colorRamp origin: origin corner: corner 
	| pixelRamp direction normal |
	
	pixelRamp := GradientFillStyle pixelRampCache at: colorRamp.
	direction := corner - origin.
	normal := direction y negated @ direction x.
	
	currentFill := 
		self primAddGradientFill: pixelRamp
			from: origin
			along: direction
			normal:  normal
			radial:  false.