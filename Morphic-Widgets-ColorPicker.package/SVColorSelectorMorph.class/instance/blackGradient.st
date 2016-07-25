as yet unclassified
blackGradient
	"Answer the black gradient. Top to bottom, transparent to black."

	^(InterpolatedGradientFillStyle colors: {Color black alpha: 0. Color black})
		origin: self innerBounds topLeft;
		direction: 0@self innerBounds height