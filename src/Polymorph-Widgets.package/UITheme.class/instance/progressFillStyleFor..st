fill-styles
progressFillStyleFor: aProgress
	"Return the progress fillStyle for the given progress morph."
	| fill |
	fill := GradientFillStyle ramp: {0.0 -> Color white. 1 ->aProgress color}.
	fill radial: false;
		origin: aProgress topLeft;
		direction: 0 @ aProgress height.
	^ fill