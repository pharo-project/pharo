tests
testCreateFile
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffles'.
	(filesystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	(filesystem workingDirectory / 'plonk') deleteAll.