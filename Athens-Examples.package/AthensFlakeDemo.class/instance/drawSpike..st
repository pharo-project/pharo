as yet unclassified
drawSpike: canvas


	|  dist |
	
	
	dist := (canvas pathTransform transform: 1@0) - (canvas pathTransform transform: 0@0).
  	dist r < 2 ifTrue: [ ^ self ].
	
	canvas
		setPaint: (Color black alpha: 0.5);
		drawShape: spike.
		
	canvas pathTransform restoreAfter: [
 		canvas pathTransform translateX: 0 Y: 	1;
			scaleBy: 0.5 + ((frame *0.1) sin * 0.1 ).
			
		canvas pathTransform  rotateByDegrees: -180.
		1 to: 2 do:[ :i |
				canvas pathTransform  rotateByDegrees: 120 + ((frame * 0.1) cos * 20).
			canvas pathTransform  restoreAfter:  [
				self drawSpike: canvas.
			]
		].

			
		self drawCircles: canvas
	]

