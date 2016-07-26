tests
testWorking
	self assert: filesystem workingDirectory fileSystem = filesystem.
	self assert: filesystem workingDirectory path = filesystem workingDirectoryPath