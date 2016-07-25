geometry
sliderThickness
	"Answer the thickness of the slider."
	
	^((self bounds isWide
		ifTrue: [self height]
		ifFalse: [self width]) // 2 max: 8) // 2 * 2 + 1