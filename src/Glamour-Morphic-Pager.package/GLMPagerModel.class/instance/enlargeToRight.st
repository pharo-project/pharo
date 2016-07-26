panes
enlargeToRight

	self virtualLastVisiblePageIndex: (self lastVisiblePageIndex + 1).
	self recomputeState.