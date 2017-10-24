fill-styles
splitterNormalFillStyleFor: aSplitter
	"Return the normal splitter fillStyle for the given splitter."
	
	|aColor|
	aColor := aSplitter paneColor.
	^(GradientFillStyle ramp: {
		0.0->aColor twiceLighter. 1.0->aColor darker})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false