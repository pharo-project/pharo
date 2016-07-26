converting
asAthensPaintOn: aCanvas
	^ aCanvas surface
		createRadialGradient:  colorRamp
		center: center
		radius: radius
		focalPoint: focalPoint 