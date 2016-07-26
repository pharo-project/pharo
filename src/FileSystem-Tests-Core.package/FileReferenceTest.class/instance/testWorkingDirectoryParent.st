tests
testWorkingDirectoryParent
	| wd |
	wd := filesystem referenceTo: Path workingDirectory.
	self assert: wd parent path size = 1.
	self assert: (wd parent path at: 1) = '..'.