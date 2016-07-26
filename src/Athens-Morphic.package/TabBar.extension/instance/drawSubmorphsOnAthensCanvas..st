*Athens-Morphic
drawSubmorphsOnAthensCanvas: anAthensCanvas
	super drawSubmorphsOnAthensCanvas: anAthensCanvas.
	self selectedTab
		ifNil: [ self drawLinesOnAthensCanvas: anAthensCanvas ]
		ifNotNil: [ self drawLinesWithSelection: anAthensCanvas ]