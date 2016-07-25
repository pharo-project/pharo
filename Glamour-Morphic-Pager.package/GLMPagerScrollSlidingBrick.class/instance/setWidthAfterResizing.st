as yet unclassified
setWidthAfterResizing
	| left right |
	
	left := pagerModel circleButtonSize * (pagerModel firstVisiblePageIndex - 1).
	right := left + self computeWidth.
	
	self setPosition: left@right