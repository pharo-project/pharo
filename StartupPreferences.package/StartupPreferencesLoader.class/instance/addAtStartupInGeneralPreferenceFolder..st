script generation
addAtStartupInGeneralPreferenceFolder: aCollection 
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesGeneralFolder 
		named: self class startupGeneralPreferencesFolderScriptName