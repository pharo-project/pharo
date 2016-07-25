tests
testDelete
	preferences parent ensureCreateDirectory.
	preferences ensureCreateFile.
	self assert: preferences exists.
	persistence delete.
	self deny: preferences exists.
	