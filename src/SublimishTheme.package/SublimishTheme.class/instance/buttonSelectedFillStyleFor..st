fill-styles-buttons
buttonSelectedFillStyleFor: aButton
	"Return the normal button fillStyle for the given button."
	
	^ SolidFillStyle color: self selectionColor.
	"| top bottom |
	
	top := self secondarySelectionColor.
	bottom := self selectionColor.

	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aButton bounds origin;
		direction: 0 @ aButton height;
		radial: false"