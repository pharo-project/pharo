fill-styles
progressBarFillStyleFor: aProgressBar
	"Return the progress bar fillStyle for the given progress bar."
	
	|aColor area|
	aColor := self progressBarColorFor: aProgressBar.
	area :=  aProgressBar bounds.
	^(GradientFillStyle ramp: {
			0.0->aColor darker duller. 0.2-> aColor darker.
			0.8->aColor twiceLighter. 1.0->aColor})	
		origin: area origin;
		direction: 0@area height;
		radial: false