paints
createCircularGradient: colorRamp center: aCenter radius: aRadius

	^ AthensBalloonRadialGradient new
		colorRamp: colorRamp;
		center: aCenter;
		radius: aRadius;
		yourself