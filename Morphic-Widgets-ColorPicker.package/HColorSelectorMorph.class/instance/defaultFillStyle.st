protocol
defaultFillStyle
	"Answer the hue gradient."

	^(GradientFillStyle colors: ((0.0 to: 359.9 by: 0.1) collect: [:a | Color h: a s: 1.0 v: 1.0]))
		origin: self topLeft;
		direction: (self bounds isWide
					ifTrue: [self width@0]
					ifFalse: [0@self height])