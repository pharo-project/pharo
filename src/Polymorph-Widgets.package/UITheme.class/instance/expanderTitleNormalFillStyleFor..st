fill-styles
expanderTitleNormalFillStyleFor: anExpanderTitle
	"Return the normal expander title fillStyle for the given expander title."
	
	|aColor|
	aColor := anExpanderTitle paneColor.
	^(GradientFillStyle ramp: {
			0.0->Color white.
			0.05-> aColor lighter.
			1.0->aColor darker})
		origin: anExpanderTitle topLeft;
		direction: 0 @ anExpanderTitle height;
		radial: false