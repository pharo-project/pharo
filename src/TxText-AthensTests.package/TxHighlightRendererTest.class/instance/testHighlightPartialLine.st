tests
testHighlightPartialLine
	"Where the selection is totally inside the layout"

	self 
		initializeRendererWithIntervalFrom: view text startPosition + 1
		to: view text startPosition + 5.
		
	renderer renderHighlightOn: self.

	self assert: drawnShapes size equals: 1.
	self assertRectangleCoversMiddleOfLine: 1.