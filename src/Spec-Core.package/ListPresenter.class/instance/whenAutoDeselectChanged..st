api-events
whenAutoDeselectChanged: aBlock
	"Set a block to value when the value of autoDeselect has changed"
	
	autoDeselect whenChangedDo: aBlock