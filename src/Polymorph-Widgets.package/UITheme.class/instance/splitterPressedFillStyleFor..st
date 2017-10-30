fill-styles
splitterPressedFillStyleFor: aSplitter
	"Return the pressed splitter fillStyle for the given splitter."
	
	|aColor|
	aColor := self glamorousBaseColorFor: aSplitter.
	^ (GradientFillStyle ramp: {0.0->aColor lighter. 0.9-> aColor})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false