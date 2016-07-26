scripting
version23PreLoad

	self isEpiceaAlreadyLoaded ifTrue: [
		self stopLogging.
		self revertAllModified: self version23OverridenPackageNames ]