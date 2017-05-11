initialization
initializeWithBaseLocator: aDirectoryFileLocator

	self initialize.
	
	baseLocator := aDirectoryFileLocator.
	self store. "Force initialization"
	headReference := self store headReference.
