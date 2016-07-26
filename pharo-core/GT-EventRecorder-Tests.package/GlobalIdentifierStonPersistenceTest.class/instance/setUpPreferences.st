running
setUpPreferences
	| memory |
	memory := FileSystem memory.
	preferences := memory workingDirectory / self folderName / 'pharoX' / 'preferences.ston'.
	preferences ensureDelete.