private-extensions-gradientfillstyle
equals: anGradientFillStyle
	<flExtensionOf: #GradientFillStyle selector: #= >
	"Answer whether equal."

	^ self pixelRamp == anGradientFillStyle pixelRamp "LRU should make identity equal"