protocol
defaultFillStyle
	"Answer the hue gradient."

	^(GradientFillStyle colors: {self color alpha: 0. self color})
		origin: self topLeft;
		direction: (self bounds isWide
					ifTrue: [self width@0]
					ifFalse: [0@self height])