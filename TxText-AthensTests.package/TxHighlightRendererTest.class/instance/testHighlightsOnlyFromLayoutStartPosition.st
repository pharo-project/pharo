tests
testHighlightsOnlyFromLayoutStartPosition
	"For a sample interval whose start is before the layout start, the highlight should start from the layout start"
		
	self 
		initializeRendererWithIntervalFrom: view text startPosition + 1
		to: view text startPosition moveDown + 5.
		
	view scrollBy: 0 @ self layout lines first height + 1.
		
	renderer renderHighlightOn: self.

	"Assert first line, which is out of view, is not highlighted"
	self assert: drawnShapes size equals: 1.
	self assertRectangle: 1 coversLeftPartOfLine: 1. "From the layout's perspective, 1 is the first one visible e.g. the second line in our selection"