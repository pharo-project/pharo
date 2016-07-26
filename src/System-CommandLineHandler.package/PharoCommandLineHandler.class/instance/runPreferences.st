private - preferences
runPreferences

	Smalltalk at: #SystemSettingsPersistence ifPresent: [:persistence | 
		persistence resumeSystemSettings].
	Smalltalk at: #StartupPreferencesLoader ifPresent: [:loader |
		loader default loadFromDefaultLocations ].
