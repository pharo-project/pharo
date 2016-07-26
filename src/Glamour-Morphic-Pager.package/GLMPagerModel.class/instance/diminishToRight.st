panes
diminishToRight

	self actualNumberOfVisiblePages = 1 ifTrue: [ ^ self ].
	self virtualFirstVisiblePageIndex: (self firstVisiblePageIndex + 1).
	self recomputeState.