fill-styles-scrollbars
scrollbarNormalButtonFillStyleFor: aScrollbar
	"Return the normal scrollbar button fillStyle for the given scrollbar."
	
	|c|
	c := self baseScrollbarColorFor: aScrollbar.
	^(GradientFillStyle ramp: {0.0->c twiceLighter. 1.0->c darker})
		origin: aScrollbar topLeft;
		direction: (aScrollbar bounds isWide
			ifTrue: [0 @ aScrollbar height]
			ifFalse: [aScrollbar width @ 0]);
		radial: false