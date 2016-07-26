fill-styles
morphTreeSplitterNormalFillStyleFor: aSplitter
	"Return the normal splitter fillStyle for the given splitter."
	
	|aColor|
	aColor := aSplitter paneColor.
	^(GradientFillStyle ramp: {
		0.0->aColor whiter whiter. 0.2->aColor lighter.
		0.8->aColor darker. 1.0->aColor blacker})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false