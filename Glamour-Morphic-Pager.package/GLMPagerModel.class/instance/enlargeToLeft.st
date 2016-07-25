panes
enlargeToLeft

	self virtualFirstVisiblePageIndex: (self firstVisiblePageIndex - 1).
	self recomputeState.