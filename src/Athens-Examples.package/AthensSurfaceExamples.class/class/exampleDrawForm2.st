examples
exampleDrawForm2
"self exampleDrawForm2"
	| surf  |
		
	surf := self newSurface: 300@300.
	
	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.

		can setShape: (0@0 corner: 300@300).
		can setPaint: Color black.
		can draw.
		can pathTransform scaleBy: 0.5;
		 translateX: 60 Y: 60. 
		(can setPaint: (self iconNamed: #helpIcon)) repeat.
		can paintTransform scaleBy: 3; rotateByDegrees: 30.
		can draw.
	].
	Display getCanvas drawImage: surf asForm  at: 0@0
