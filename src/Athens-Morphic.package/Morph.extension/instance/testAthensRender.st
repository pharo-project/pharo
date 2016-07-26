*Athens-Morphic
testAthensRender

| surf |
	surf := AthensCairoSurface extent: Display extent.

	surf drawDuring: [:can |
"		can pathTransform scaleBy: 0.5; rotateByDegrees: 30."
		self fullDrawOnAthensCanvas: can ].

	Display getCanvas drawImage: 	surf asForm  at: 0@0
