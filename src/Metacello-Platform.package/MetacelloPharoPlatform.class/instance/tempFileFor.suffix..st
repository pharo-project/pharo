file system
tempFileFor: aName suffix: aSuffixString
	^ (FileLocator temp asFileReference
		/ (FileReference newTempFilePrefix: aName suffix: aSuffixString) basename)
		fullName