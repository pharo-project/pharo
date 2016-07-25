tests
testHasFilesTyped
	self assert: (self commandLine hasFileWithExtension: #txt).
	self deny: (self commandLine hasFileWithExtension: #foo).