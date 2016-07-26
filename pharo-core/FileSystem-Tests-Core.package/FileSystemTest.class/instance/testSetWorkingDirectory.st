tests
testSetWorkingDirectory
	| cwd |
	filesystem workingDirectoryPath: (Path / 'plonk').
	cwd := filesystem workingDirectoryPath.
	self assert: cwd isAbsolute.
	self assert: (cwd at: 1) = 'plonk'