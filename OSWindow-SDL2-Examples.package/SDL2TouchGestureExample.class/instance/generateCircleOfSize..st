drawing
generateCircleOfSize: anInteger
	^ renderer athensSurface
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: anInteger negated @ 0;
				ccwArcTo: 0 @ anInteger angle: 90 degreesToRadians;
				ccwArcTo: anInteger @ 0 angle: 90 degreesToRadians;
				ccwArcTo: 0 @ anInteger negated angle: 90 degreesToRadians;
				ccwArcTo: anInteger negated @ 0 angle: 90 degreesToRadians ]