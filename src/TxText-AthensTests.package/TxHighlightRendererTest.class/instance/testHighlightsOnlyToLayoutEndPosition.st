tests
testHighlightsOnlyToLayoutEndPosition
	"For a sample interval whose end is after the layout end, the highlight should end at the layout end"
	
	| lastVisibleLine lastVisibleLineIndex |
	lastVisibleLine := self layout lines last.
	lastVisibleLineIndex := self layout lines size.
	self 
		initializeRendererWithIntervalFrom: lastVisibleLine startPosition + 1
		to: lastVisibleLine endPosition moveDown moveToLineEnd.

	renderer renderHighlightOn: self.

	"Assert second line, which is out of view, is not highlighted"
	self assert: drawnShapes size equals: 1.
	self assertRectangle: 1 coversRightPartOfLine: lastVisibleLineIndex.