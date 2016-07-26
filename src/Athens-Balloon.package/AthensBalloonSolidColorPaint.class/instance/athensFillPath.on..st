rendering
athensFillPath: athensBalloonPath on: anAthensCanvas 

	^ anAthensCanvas surface 
		fillBezierShape: athensBalloonPath contoursForFilling  
		color: color.
		