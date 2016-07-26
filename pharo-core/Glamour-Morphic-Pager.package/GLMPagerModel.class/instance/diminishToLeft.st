panes
diminishToLeft

	self actualNumberOfVisiblePages = 1 ifTrue: [ ^ self ].
	self virtualLastVisiblePageIndex: (self lastVisiblePageIndex - 1).
	self recomputeState.