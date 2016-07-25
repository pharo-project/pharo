tests
testIsDirectory
	self assert: (filesystem isDirectory: FileLocator imageDirectory resolve path)