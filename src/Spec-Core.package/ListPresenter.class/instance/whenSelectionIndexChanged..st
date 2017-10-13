api-events
whenSelectionIndexChanged: aBlock
	"Set a block to value when the selection index has changed"
	
	selectionHolder index whenChangedDo: aBlock