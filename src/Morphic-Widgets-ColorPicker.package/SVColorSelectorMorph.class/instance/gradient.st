as yet unclassified
gradient
	"Answer the base gradient."

	|b|
	b := self innerBounds.
	^(GradientFillStyle colors: {Color white. self color})
		origin: b topLeft;
		direction: (b width@0)