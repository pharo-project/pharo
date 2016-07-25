fill-styles
splitterPressedFillStyleFor: aSplitter
	"Return the pressed splitter fillStyle for the given splitter."
	
	|aColor|
	aColor := aSplitter paneColor twiceDarker.
	^(GradientFillStyle ramp: {
			0.0->Color white. 0.4-> aColor twiceLighter.
			1.0->aColor darker duller})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false