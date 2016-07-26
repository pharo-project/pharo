actions
lookInGeneralPreferencesFolder

	^ StartupPreferencesLoader preferencesGeneralFolder 
		ifNotNil: [ :folder | self lookInFolder:  folder forFilesMatching: '*.st' ].