volume list and pattern
veryDeepFixupWith: deepCopier
	super veryDeepFixupWith: deepCopier.
	volumeListIndex := 1.
	self directory: FileSystem disk workingDirectory.
	self updateFileList