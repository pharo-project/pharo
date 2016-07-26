creation
ensureCreateDirectory: aDirectoryName
	^ self 
		fileEntryAt: aDirectoryName
		put: (self class named: aDirectoryName)