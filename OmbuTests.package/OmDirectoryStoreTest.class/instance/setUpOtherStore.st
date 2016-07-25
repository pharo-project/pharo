resources
setUpOtherStore
	
	| directory |
	directory := (FileSystem workingDirectory / #ombuTestOtherStore) ensureCreateDirectory.
	
	self auxiliaryStoreNamed: #otherAuxiliary in: directory.

	^ OmDirectoryStore in: directory