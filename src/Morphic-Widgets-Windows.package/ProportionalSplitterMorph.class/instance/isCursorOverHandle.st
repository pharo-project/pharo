testing
isCursorOverHandle
	^ self class showSplitterHandles not or: [self handleRect containsPoint: ActiveHand cursorPoint]