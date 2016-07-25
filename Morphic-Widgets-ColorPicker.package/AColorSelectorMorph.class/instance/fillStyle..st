visual properties
fillStyle: fillStyle
	"If it is a color then override with gradient."
	
	fillStyle isColor
		ifTrue: [self color: fillStyle]
		ifFalse: [super fillStyle: fillStyle]