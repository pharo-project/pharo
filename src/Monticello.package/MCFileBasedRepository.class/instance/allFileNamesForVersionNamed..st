accessing
allFileNamesForVersionNamed: aString
	^ self filterFileNames: self readableFileNames forVersionNamed: aString