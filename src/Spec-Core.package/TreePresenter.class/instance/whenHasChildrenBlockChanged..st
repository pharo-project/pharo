api-events-compatibility
whenHasChildrenBlockChanged: aBlock
	"Set a block to perform when a new hasChildrenBlock is set"

	hasChildrenBlockHolder whenChangedDo: aBlock