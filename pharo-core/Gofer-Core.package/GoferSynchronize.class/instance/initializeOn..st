initialization
initializeOn: aGofer
	super initializeOn: aGofer disablePackageCache.
	MCFileBasedRepository flushAllCaches.
	cacheReferences := self gofer allResolvedIn: self cacheRepository