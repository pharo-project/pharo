api-events
whenReadSelectionIsChanged: aBlock
	"Set a block to perform when the read selection block changed"
	
	readSelectionHolder whenChangedDo: aBlock