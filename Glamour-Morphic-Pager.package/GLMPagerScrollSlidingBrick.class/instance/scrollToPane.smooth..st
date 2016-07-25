slider-scrolling
scrollToPane: anIndex smooth: isSmooth

	self scrollTo: (pagerModel convertIndexToValue: anIndex) smooth: isSmooth.