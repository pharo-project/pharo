border-styles
drawTextAdornmentFor: aPluggableTextMorph color: aColor on: aCanvas
	"Indicate edit status for the given morph."
	
	|bounds size fillStyle|
	bounds := aPluggableTextMorph innerBounds.
	size := 25.
	fillStyle := (GradientFillStyle ramp: {
			0.0->(self backgroundColor alpha: 0.01).
			0.8->aColor.
			1.0->aColor})
		origin: bounds topRight - (size@0);
		direction: (size @ size negated) // 4;
		radial: false.
	aCanvas
		drawPolygon:  {bounds topRight. bounds topRight + (0@size). bounds topRight - (size@0)}
		fillStyle: fillStyle