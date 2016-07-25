file reader services
removeLineFeeds: fullName
	| fileContents |
	fileContents := ((FileStream readOnlyFileNamed: fullName) wantsLineEndConversion: true) contentsOfEntireFile.
	(FileStream newFileNamed: fullName) 
		nextPutAll: fileContents;
		close.