api-events
whenSelectionChanged: aBlock
	"Set a block to perform when the selection is changed"
	
	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	selectionHolder whenChangedDo: aBlock.