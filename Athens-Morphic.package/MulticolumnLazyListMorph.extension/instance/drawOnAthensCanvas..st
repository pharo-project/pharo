*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	self getListSize = 0 ifTrue:[ ^self ].

	self setColumnWidthsFor: anAthensCanvas.
	self adjustWidth.
	
	super drawOnAthensCanvas: anAthensCanvas