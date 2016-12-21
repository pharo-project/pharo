api
possibleEntities: aBlock
	"<api: #block getter: #possibleEntities registration: #whenPossibleEntitiesChanged:>"
	"Set the block used to return a collection of all possible entity choices"
	possibleEntities value: aBlock.
	entityText entryCompletion: self entitiesEntryCompletion.