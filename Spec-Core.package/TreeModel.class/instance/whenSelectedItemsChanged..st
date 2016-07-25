api-events
whenSelectedItemsChanged: aBlock
	"Set a block to perform when a new item is selected"

	selectionHolder whenChangedDo: [ :col | aBlock cull: (col collect: [:e | e content ]) ]