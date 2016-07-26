color mapping
pixelValueFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	self hasNonStandardPalette
		ifTrue:[^self colormapFromARGB mapPixel: (aColor pixelValueForDepth: 32)]
		ifFalse:[^aColor pixelValueForDepth: self depth]