tests
testHighlightTwoLines
	"For a sample interval which is on two lines, which are totally inside the layout, the following should be highlighted:
		- first line from interval start to view width
		- second line from line start to interval end"
		
	self 
		initializeRendererWithIntervalFrom: view text startPosition + 1
		to: view text startPosition moveDown + 5.
		
	renderer renderHighlightOn: self.

	self assert: drawnShapes size equals: 2.	
	self assertRectangle: 1 coversRightPartOfLine: 1.
	self assertRectangle: 2 coversLeftPartOfLine: 2.