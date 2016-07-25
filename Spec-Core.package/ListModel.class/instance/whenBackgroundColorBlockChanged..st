api-events
whenBackgroundColorBlockChanged: aBlock
	"Set a block to value when the backgroundColorBlock has changed"
	
	backgroundColorBlock whenChangedDo: aBlock