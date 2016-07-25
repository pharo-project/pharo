fill-styles
progressBarProgressFillStyleFor: aProgressBar
	"Return the progress bar progress fillStyle for the given progress bar."
	
	|aColor area|
	aColor := self progressBarProgressColorFor: aProgressBar.
	area :=  aProgressBar innerBounds.
	^(GradientFillStyle ramp: {
			0.0->(aColor alphaMixed: 0.3 with: (Color white alpha: aColor alpha)).
			0.2-> aColor twiceLighter.
			0.8->aColor darker.
			1.0->aColor darker duller})	
		origin: area origin;
		direction: 0@area height;
		radial: false