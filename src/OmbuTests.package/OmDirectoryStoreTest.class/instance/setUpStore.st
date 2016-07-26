resources
setUpStore	
	
	| directory |
	directory := (FileSystem workingDirectory / #ombuTestStore) ensureCreateDirectory.
	
	self auxiliaryStoreNamed: #auxiliary in: directory.

	^ OmDirectoryStore in: directory