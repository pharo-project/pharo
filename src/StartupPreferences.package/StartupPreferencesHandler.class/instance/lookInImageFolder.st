actions
lookInImageFolder

	^ self 
		lookInFolder: FileSystem workingDirectory 
		forFilesMatching: StartupPreferencesLoader startupScriptName.