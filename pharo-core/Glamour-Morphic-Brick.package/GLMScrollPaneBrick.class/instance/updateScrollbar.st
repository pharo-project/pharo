scrollpane-updating
updateScrollbar

	self verticalScrollbar ifNotNil: [ :b | b updatePosition ]