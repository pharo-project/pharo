initialization
newStore
	^ OmFileStore 
		named: 'tmp', UUID new asString36
		inDirectory: (FileSystem workingDirectory / 'epicea-builder') ensureCreateDirectory