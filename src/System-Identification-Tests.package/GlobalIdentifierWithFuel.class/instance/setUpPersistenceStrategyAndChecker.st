running
setUpPersistenceStrategyAndChecker
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / self actualPreferencesFileName.
	preferences ensureDelete.
	identifier persistenceStrategy: GlobalIdentifierPersistence fuel.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy checker: (self newChecker answer: [ FileLocator workingDirectory exists ]; yourself).
