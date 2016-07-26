event-handling
onPagesNumberChanged

	self maxPanes: pagerModel numberOfVisiblePages.
	self doLayoutForce.
	self scrollToPane: pagerModel firstVisiblePageIndex smooth: false
	