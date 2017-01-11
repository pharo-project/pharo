initialization
setUpPersistenceStrategy
	oldPreferences := FileLocator workingDirectory / self folderName / 'pharoX' / self oldPreferencesFileName.
	oldPreferences ensureDelete.
	identifier persistenceStrategy preferences: preferences.
	identifier persistenceStrategy previous preferences: oldPreferences