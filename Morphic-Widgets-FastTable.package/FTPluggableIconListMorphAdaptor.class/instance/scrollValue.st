accessing
scrollValue
	self flag: #todo. "x is always zero because horizontal scroll bar is not yet implemented"
	^ 0@(self verticalScrollBar value)