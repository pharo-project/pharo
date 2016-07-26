scrolling
scrollToPane: anIndex smooth: isSmooth callback: aBlock

	self scrollTo: (pagerModel convertIndexToValue: anIndex) smooth: isSmooth callback: aBlock.