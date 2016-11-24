mc related utils
saveInToReloadCachePackage: aWorkingCopy with: aMessageString
	self
		savePackage: aWorkingCopy
		in: self class defaultMCWaitingFolder
		with: aMessageString