instance creation
forFilenameExtension: extension ifAbsent: block
	^ ExtensionsMap 
		at: extension 
		ifAbsent: block