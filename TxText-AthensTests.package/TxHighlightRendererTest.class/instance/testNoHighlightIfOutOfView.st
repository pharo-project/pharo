tests
testNoHighlightIfOutOfView
	
	| lastVisibleLine lastVisibleLineIndex |
	lastVisibleLine := self layout lines last.
	lastVisibleLineIndex := self layout lines size.
	self 
		initializeRendererWithIntervalFrom: lastVisibleLine startPosition moveDown + 1
		to: lastVisibleLine endPosition moveDown moveDown moveToLineEnd.

	renderer renderHighlightOn: self.

	self assert: drawnShapes isEmpty.