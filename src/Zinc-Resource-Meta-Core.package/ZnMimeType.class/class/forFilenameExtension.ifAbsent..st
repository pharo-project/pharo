instance creation
forFilenameExtension: extension ifAbsent: block
	^ self extensionsMap 
		at: extension 
		ifAbsent: block