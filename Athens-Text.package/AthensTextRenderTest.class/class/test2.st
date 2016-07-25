as yet unclassified
test2
	|  t  c surf |
	t := self testText. 
	t addAttribute: (AthensTextBackground new color: (Color green alpha: 0.3 )) from: 1 to: 267.

	c := AthensTextComposer scan: t for: AthensTextScanner new.

	surf := self surfaceClass extent: Display extent.
	
	surf drawDuring: [ :canvas | 
		
		canvas setPaint: (Color white ).
		canvas drawShape: (0@0 corner: 300@300).
		canvas pathTransform scaleBy: 1.

		canvas pathTransform translateX: 300 Y: 0.

		canvas setPaint: Color yellow.
		canvas pathTransform rotateByDegrees: 0.
		c renderOn: canvas
	].

	Display getCanvas translucentImage: surf form at: 0@0

