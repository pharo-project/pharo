api-events
whenSortingBlockChanged: aBlock
	"Set a block to value when the sorting block has changed"
	
	sortingBlockHolder whenChangedDo: aBlock