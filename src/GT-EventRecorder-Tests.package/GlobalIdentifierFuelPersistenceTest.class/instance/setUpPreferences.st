running
setUpPreferences
	preferences := FileLocator workingDirectory / self folderName / 'pharoX' / 'preferences.fuel'.
	preferences ensureDelete.