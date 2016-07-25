scripting
version33PreLoad

	self isEpiceaAlreadyLoaded ifTrue: [
		self stopLogging.
		self revertAllModified: self version33OverridenPackageNames ]