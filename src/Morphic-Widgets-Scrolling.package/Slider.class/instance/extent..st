geometry
extent: newExtent
	newExtent = bounds extent ifTrue: [^ self].
	bounds isWide
		ifTrue: [super extent: (newExtent x max: self sliderThickness * 2) @ newExtent y]
		ifFalse: [super extent: newExtent x @ (newExtent y max: self sliderThickness * 2)].
	self removeAllMorphs; initializeSlider