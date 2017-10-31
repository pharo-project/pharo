tests
testCreateDirectory
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffle'.
	(filesystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	(filesystem workingDirectory / 'plonk' )deleteAll.