tests
testNonExistentFileSize
	| base file1 file2 |
	
	base := filesystem workingDirectory.
	file1 := (base / 'file1') ensureCreateFile.
	file2 := (base / 'file2').
	self markForCleanup: base / 'file1'.
	
	self shouldnt: [ file1 size ] raise: FileDoesNotExist.
	self should: [ file2 size ] raise: FileDoesNotExist