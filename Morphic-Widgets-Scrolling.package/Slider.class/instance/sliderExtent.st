geometry
sliderExtent
	^ bounds isWide
		ifTrue: [self sliderThickness @ self innerBounds height]
		ifFalse: [self innerBounds width @ self sliderThickness]