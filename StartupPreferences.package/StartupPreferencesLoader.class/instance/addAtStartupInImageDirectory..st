script generation
addAtStartupInImageDirectory:  aCollection
	self 
		addAtStartup: aCollection 
		inDirectory: FileSystem workingDirectory 
		named: self class startupScriptName
