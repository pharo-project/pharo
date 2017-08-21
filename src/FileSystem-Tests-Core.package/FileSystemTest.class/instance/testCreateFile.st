tests
testCreateFile
	| path |
	path := FileSystem workingDirectory / 'plonk' / 'griffles'.
	(FileSystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	(FileSystem workingDirectory / 'plonk') deleteAll.