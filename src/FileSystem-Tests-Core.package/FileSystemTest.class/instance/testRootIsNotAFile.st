tests
testRootIsNotAFile
	self deny: (filesystem isFile: Path root)