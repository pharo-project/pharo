fill-styles-buttons
buttonSelectedFillStyleFor: aButton
	"Return the normal button fillStyle for the given button."
	
	| top bottom |
	top := self glamorousLightSelectionColorFor: aButton.
	bottom := self glamorousBaseColorFor: aButton.

	^(GradientFillStyle ramp: {
			0.0->top.
			0.7->bottom.})
		origin: aButton bounds origin;
		direction: 0 @ aButton height;
		radial: false