script generation
addAtStartupInPreferenceVersionFolder: aCollection named: fileName
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesVersionFolder 
		named: fileName