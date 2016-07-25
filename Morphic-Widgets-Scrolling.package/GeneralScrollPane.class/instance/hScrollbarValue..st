scrollbars
hScrollbarValue: scrollValue
	"Set the offset of the scroller to match the 0.0-1.0 scroll value."
	
	|r|
	r := self scrollTarget width - self scrollBounds width max: 0.
	self scroller
		offset: (r * scrollValue) rounded @ self scroller offset y