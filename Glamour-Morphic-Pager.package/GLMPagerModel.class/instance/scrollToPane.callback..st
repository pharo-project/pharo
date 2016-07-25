scrolling
scrollToPane: pageIndex callback: aBlock
	"scrolls to specified pane specified by its index"

	self scrollTo: (self convertIndexToValue: pageIndex) smooth: true callback: aBlock
	