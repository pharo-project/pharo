paints
createRadialGradient: colorRamp center: aCenter radius: aRadius
	"by default, focal point coincede with center"
	^ self createRadialGradient: colorRamp center: aCenter radius: aRadius focalPoint: aCenter
