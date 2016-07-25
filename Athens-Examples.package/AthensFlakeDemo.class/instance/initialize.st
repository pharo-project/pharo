as yet unclassified
initialize 

	super initialize.
	self extent: self defaultExtent.

	surface := AthensCairoSurface extent: self extent.

	circle := surface createPath: [:builder |
		builder
			absolute;
			moveTo: -1 @ 0 ; 
			ccwArcTo: 0@ 1 angle: 90 degreesToRadians ;
			ccwArcTo: 1@0 angle: 90 degreesToRadians ;
			ccwArcTo: 0@ -1 angle: 90 degreesToRadians ;
			ccwArcTo: -1@0 angle:  90 degreesToRadians 
	].

	spike := surface createPath: [:builder |
		
		builder
			absolute;
			moveTo:  -0.1 @ 0;
			lineTo: -0.05 @ 1;
			lineTo: 0.05 @ 1;
			lineTo: 0.1 @ 0		
		].

	frame := 1.
	self startStepping.
	
