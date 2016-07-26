running
setUp
	super setUp.
	identifier := GlobalIdentifier basicNew initialize.
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self fileName.
	preferences ensureDelete.
	self setUpPersistenceStrategy