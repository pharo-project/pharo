tests
testCreateDirectoryNotCreateParent
	| path |
	path := FileSystem workingDirectory / 'plonk' / 'griffle'.
	self should:[path createDirectory] raise: DirectoryDoesNotExist.
	self assert: path exists not.