tests
testCreateDirectory
	| path |
	path := FileSystem workingDirectory / 'plonk' / 'griffle'.
	(FileSystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	(FileSystem workingDirectory / 'plonk' )deleteAll.