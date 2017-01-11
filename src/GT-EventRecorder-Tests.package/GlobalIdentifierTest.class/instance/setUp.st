running
setUp
	super setUp.
	identifier := GlobalIdentifier basicNew initialize.
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	self setUpPersistenceStrategy