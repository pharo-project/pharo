tests
testSetRelativeWorkingDirectory
	self 
		should: [filesystem workingDirectoryPath: (Path * 'plonk')]
		raise: Error
	