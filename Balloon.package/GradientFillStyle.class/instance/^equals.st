comparing
= anGradientFillStyle
	"Answer whether equal."

	^super = anGradientFillStyle
		and: [self pixelRamp == anGradientFillStyle pixelRamp] "LRU should make identity equal"