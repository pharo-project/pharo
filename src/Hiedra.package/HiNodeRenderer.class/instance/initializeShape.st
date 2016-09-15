initialization
initializeShape
	nodeShape := athensCanvas createPath: [ :builder | 
		builder
			absolute;
			moveTo: -1 @ 0;
			ccwArcTo: 0 @ 1 angle: 90 degreesToRadians;
			ccwArcTo: 1 @ 0 angle: 90 degreesToRadians;
			ccwArcTo: 0 @ -1 angle: 90 degreesToRadians;
			ccwArcTo: -1 @ 0 angle: 90 degreesToRadians ]

