tests
testEnsureDirectory
	persistence ensureDirectory.
	self assert: preferences parent exists.
	self deny: preferences exists.
	