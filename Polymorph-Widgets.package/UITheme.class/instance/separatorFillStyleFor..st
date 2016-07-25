fill-styles
separatorFillStyleFor: aSeparator
	"Return the separator fillStyle for the given separator."
	
	|aColor|
	aColor := aSeparator paneColor.
	^(GradientFillStyle ramp: {
		0.0->aColor whiter whiter. 0.2->aColor lighter.
		0.8->aColor darker. 1.0->aColor blacker})
		origin: aSeparator topLeft;
		direction: (aSeparator bounds isWide
			ifTrue: [0 @ aSeparator height]
			ifFalse: [aSeparator width @ 0]);
		radial: false