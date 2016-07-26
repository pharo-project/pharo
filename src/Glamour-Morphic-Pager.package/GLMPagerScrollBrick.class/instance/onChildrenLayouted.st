brick-layout-events
onChildrenLayouted
	super onChildrenLayouted.
	self band translateXTo: (self convertScrollValue: (self pagerModel convertIndexToValue: self pagerModel firstVisiblePageIndex))