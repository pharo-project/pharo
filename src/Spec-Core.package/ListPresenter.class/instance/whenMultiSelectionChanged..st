api-events
whenMultiSelectionChanged: aBlock
	"Set a block to value when the multiSelection value has changed"
	
	multiSelection whenChangedDo: aBlock