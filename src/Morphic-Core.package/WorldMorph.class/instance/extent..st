geometry
extent: aPoint
	super extent: aPoint.
	worldState viewBox
		ifNotNil: [ 
			worldState canvas: nil.
			worldState viewBox: bounds ]