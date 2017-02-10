initialization
initializeWithGlobalName: aName fileReference: aFileReference

	self initialize.
	
	globalName := aName.
	fileReference := aFileReference.
	mustRefresh := fileReference exists. "Late file read"
