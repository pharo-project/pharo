fill-styles
taskbarFillStyleFor: aTaskbar
	"Return the taskbar fillStyle for the given taskbar."
	
	|aColor|
	aColor := aTaskbar color.
	^(GradientFillStyle ramp: {
			0.0->(aColor alphaMixed: 0.3 with: (Color white alpha: aColor alpha)).
			0.2-> aColor twiceLighter.
			0.8->aColor darker.
			1.0->aColor darker duller})	
		origin: aTaskbar position;
		direction: 0@27;
		radial: false