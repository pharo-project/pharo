script generation
addAtStartupInPreferenceVersionFolder: aCollection 
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesVersionFolder 
		named: self class startupPreferencesVersionFolderScriptName