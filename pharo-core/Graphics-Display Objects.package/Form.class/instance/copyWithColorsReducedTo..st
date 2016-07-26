converting
copyWithColorsReducedTo: nColors
	"Note: this has not been engineered.
	There are better solutions in the literature."
	| palette colorMap |
	palette := self reducedPaletteOfSize: nColors.
	colorMap := (1 to: (1 bitShift: depth)) collect:
		[:i | | pc closest | pc := Color colorFromPixelValue: i-1 depth: depth.
		closest := palette detectMin: [:c | c diff: pc].
		closest pixelValueForDepth: depth].
	^ self deepCopy copyBits: self boundingBox from: self at: 0@0 colorMap: (colorMap as: Bitmap)
		