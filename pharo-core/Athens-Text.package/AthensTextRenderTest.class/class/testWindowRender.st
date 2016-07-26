as yet unclassified
testWindowRender
	| surf |
	surf := self surfaceClass extent: Display extent.

	surf drawDuring: [ :canvas | 
		
		canvas pathTransform scaleBy: 1.
		canvas fullDrawMorph: self testWindow.

	].

	Display getCanvas translucentImage: surf asForm at: 0@0

