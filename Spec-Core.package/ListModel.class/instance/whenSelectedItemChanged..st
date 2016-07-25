api-events
whenSelectedItemChanged: aBlock
	"Set a block to value when the select item is changed"
	
	selectionHolder selection whenChangedDo: aBlock