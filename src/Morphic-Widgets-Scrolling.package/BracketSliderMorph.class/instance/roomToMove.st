geometry
roomToMove
	"Allow to run off the edges a bit."
	
	^self bounds isWide
		ifTrue: [self totalSliderArea insetBy: ((self sliderThickness // 2@0) negated corner: (self sliderThickness // 2 + 1)@0)]
		ifFalse: [self totalSliderArea insetBy: (0@(self sliderThickness // 2) negated corner: 0@(self sliderThickness // 2 - (self sliderThickness \\ 2) + 1))]