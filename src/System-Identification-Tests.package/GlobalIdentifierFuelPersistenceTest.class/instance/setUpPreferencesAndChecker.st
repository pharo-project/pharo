running
setUpPreferencesAndChecker
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / 'preferences.fuel'.
	preferences ensureDelete.
	checker := GlobalIdentifierPersistenceMockChecker new answer: [ FileLocator workingDirectory exists ].
