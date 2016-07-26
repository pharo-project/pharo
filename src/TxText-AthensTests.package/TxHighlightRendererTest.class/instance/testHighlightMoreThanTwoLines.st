tests
testHighlightMoreThanTwoLines
	"Where the selection is totally inside the layout"
	
	self 
		initializeRendererWithIntervalFrom: view text startPosition + 1
		to: view text startPosition moveDown moveDown + 5.
		
	renderer renderHighlightOn: self.

	self assert: drawnShapes size equals: 3.
	self assertRectangle: 1 coversRightPartOfLine: 1.
	self assertRectangle: 2 coversFullViewWidthOfLine: 2.
	self assertRectangle: 3 coversLeftPartOfLine: 3.