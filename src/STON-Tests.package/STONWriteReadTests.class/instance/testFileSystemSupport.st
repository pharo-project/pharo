tests
testFileSystemSupport
	| fileReferences fileLocators |
	fileReferences := STON listClass withAll: { 
		FileLocator image asFileReference.
		FileLocator workingDirectory asFileReference.
		(FileLocator home / 'foo.txt') asFileReference }.
	self serializeAndMaterialize: fileReferences.
	fileLocators := STON listClass withAll: { 
		FileLocator image.
		FileLocator workingDirectory.
		FileLocator home / 'foo.txt' }.
	self serializeAndMaterialize: fileLocators.