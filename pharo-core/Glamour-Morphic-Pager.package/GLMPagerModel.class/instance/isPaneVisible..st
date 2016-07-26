panes
isPaneVisible: aNumber

	^ (aNumber >= self firstVisiblePageIndex) & (aNumber <= self lastVisiblePageIndex)