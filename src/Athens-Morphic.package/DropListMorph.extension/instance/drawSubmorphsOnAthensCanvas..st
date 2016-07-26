*Athens-Morphic
drawSubmorphsOnAthensCanvas: anAthensCanvas
	super 	drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOnAthensCanvas: anAthensCanvas].