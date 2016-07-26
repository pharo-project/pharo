compatibility tests
testFileSystem
	locator := FileLocator image.
	self assert: (locator fileSystem isKindOf: FileSystem)