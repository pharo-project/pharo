comparing
= aGradientFillStyle
	"Answer whether equal."

	^super = aGradientFillStyle
		and: [self extent = aGradientFillStyle extent]