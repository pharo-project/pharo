initialization
initializeWithBaseLocator: aDirectoryFileLocator

	self initialize.
	
	baseLocator := aDirectoryFileLocator.
	self store. "Force initialization"
	
	alternativeStore := OmDirectoryStore in: baseLocator.
	
	self subscribeToSnapshotDone.
	
	