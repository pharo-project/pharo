initialization
initializeWithBaseLocator: aDirectoryFileLocator

	self initialize.
	
	baseLocator := aDirectoryFileLocator.
	self reset.
	
	alternativeStore := OmDirectoryStore in: baseLocator 
	
	