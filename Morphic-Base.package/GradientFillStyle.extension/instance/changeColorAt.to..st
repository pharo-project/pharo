*Morphic-Base-Balloon
changeColorAt: rampIndex to: aColor
	| ramp |
	ramp := colorRamp deepCopy.
	(ramp at: rampIndex) value: aColor.
	
	colorRamp := ramp.
	isTranslucent := nil.
	pixelRamp := nil.