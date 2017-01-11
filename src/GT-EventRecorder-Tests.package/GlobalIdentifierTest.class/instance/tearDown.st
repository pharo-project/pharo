running
tearDown
	super tearDown.
	preferences ensureDelete.
	(FileLocator workingDirectory / self folderName) 
		ensureDeleteAllChildren; 
		ensureDelete.