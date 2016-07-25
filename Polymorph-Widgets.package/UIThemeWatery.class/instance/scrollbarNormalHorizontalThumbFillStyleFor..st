fill-styles-scrollbars
scrollbarNormalHorizontalThumbFillStyleFor: aScrollbar
	"Return the normal scrollbar horizontal thumb fillStyle for the given scrollbar."
	
	|s l m r|
	s := aScrollbar slider.
	l := self scrollbarThumbLeftForm.
	m := self scrollbarThumbHorizontalMiddleForm.
	r := self scrollbarThumbRightForm.
	^CompositeFillStyle fillStyles: {
		(ImageFillStyle form: l) origin: s topLeft.
		(AlphaInfiniteForm with: m)
			origin: s topLeft + (l width@0);
			extent: (s width - l width - r width)@m height.
		(ImageFillStyle form: r)
			origin: s topRight - (r width@0)}