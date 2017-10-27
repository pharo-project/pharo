fill-styles-scrollbars
scrollbarNormalVerticalThumbFillStyleFor: aScrollbar
	"Return the normal scrollbar vertical thumb fillStyle for the given scrollbar."
	
	|s t m b|
	s := aScrollbar slider.
	t := self scrollbarThumbTopForm.
	m := self scrollbarThumbVerticalMiddleForm.
	b := self scrollbarThumbBottomForm.
	^CompositeFillStyle fillStyles: {
		(ImageFillStyle form: t) origin: s topLeft.
		(AlphaInfiniteForm with: m)
			origin: s topLeft + (0@t height);
			extent: m width@(s height - t height - b height).
		(ImageFillStyle form: b)
			origin: s bottomLeft - (0@b height)}