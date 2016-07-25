scrollbars
vScrollbarValue: scrollValue
	"Set the offset of the scroller to match the 0.0-1.0 scroll value."
	
	|r|
	r := self scrollTarget height - self scrollBounds height max: 0.
	self scroller
		offset: self scroller offset x @ (r * scrollValue) rounded