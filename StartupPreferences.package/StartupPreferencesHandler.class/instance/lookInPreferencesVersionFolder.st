actions
lookInPreferencesVersionFolder

	^ StartupPreferencesLoader preferencesVersionFolder 
		ifNotNil: [ :folder | 
			self 
				lookInFolder: folder
				forFilesMatching: '*.st' ].