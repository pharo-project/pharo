initialization
defaultSliderFillStyle
	"Answer the hue gradient."

	^(GradientFillStyle colors: {Color white. Color black})
		origin: self topLeft;
		direction: (self bounds isWide
					ifTrue: [self width@0]
					ifFalse: [0@self height])