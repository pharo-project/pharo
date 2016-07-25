paints
createLinearGradient: aColorRamp start: aStartPoint stop: aStopPoint
	^ AthensCairoGradientPaint 
		createLinearGradient: aColorRamp 
		start: aStartPoint 
		stop: aStopPoint
	