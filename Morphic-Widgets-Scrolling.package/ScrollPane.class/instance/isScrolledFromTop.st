geometry testing
isScrolledFromTop
	"Have the contents of the pane been scrolled, so that the top of the contents are not visible?"
	^scroller offset y > 0
