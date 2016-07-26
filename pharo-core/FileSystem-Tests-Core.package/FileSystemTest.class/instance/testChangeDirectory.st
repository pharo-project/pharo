tests
testChangeDirectory
	| cwd |
	filesystem workingDirectoryPath: Path / 'plonk'.
	filesystem changeDirectory: 'griffle'.
	cwd := filesystem workingDirectoryPath.
	self assert: cwd isAbsolute.
	self assert: (cwd at: 1) = 'plonk'.
	self assert: (cwd at: 2) = 'griffle'.
