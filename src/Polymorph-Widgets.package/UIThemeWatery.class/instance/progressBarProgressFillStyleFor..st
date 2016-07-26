fill-styles
progressBarProgressFillStyleFor: aProgressBar
	"Return the progress bar progress fillStyle for the given progress bar."
	
	|area c|
	c := self progressBarProgressColorFor: aProgressBar.
	area :=  aProgressBar bounds.
	^(GradientFillStyle ramp: {
			0.0->c twiceDarker.
			0.2->c twiceLighter lighter.
			0.3->c twiceLighter lighter.
			0.4->c darker.
			0.6->c twiceLighter lighter.
			1.0->Color white})
		origin: area origin;
		direction: 0@area height;
		radial: false