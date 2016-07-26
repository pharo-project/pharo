updating
updatePanes
	self basicUpdateSizing.
	self layoutPanes.
	self hideOrShowScrollBar.
	self setScrollDeltas.
	self scrollToRight