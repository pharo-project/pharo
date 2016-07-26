paints
createRadialGradient: colorRamp center: aCenter radius: aRadius focalPoint: fp
	^AthensCairoGradientPaint	
		radialBetween: fp
		extending: 0
		and: aCenter
		extending: aRadius
		withColorRamp: colorRamp