running
setUpPreferencesAndChecker
	| memory |
	memory := FileSystem memory.
	preferences := memory workingDirectory / self folderName / 'pharoX' / 'preferences.ston'.
	preferences ensureDelete.
	checker := GlobalIdentifierPersistenceMockChecker new answer: [ memory workingDirectory exists ].
