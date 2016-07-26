drawing
drawOnAthensCanvas: anAthensCanvas
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	| verts polygonPath |
	verts := self getVertices.
	polygonPath := anAthensCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: verts first.
			verts allButFirstDo: [ :pt | builder lineTo: pt ].
			"builder close" ].
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas setShape: polygonPath.
	anAthensCanvas draw.
	self drawBorderOnAthensCanvas: anAthensCanvas
