scrollpane-updating
update

	self scrollDown: 0.
	self scrollUp: 0.
	self privateScrollPosition: self band brickBounds top.
	self band onChildrenLayouted