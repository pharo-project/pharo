tests
testEnsureDirectoryExists
	| path |
	path := Path * 'plonk'.
	self markForCleanup: path.
	filesystem createDirectory: path.
	filesystem ensureCreateDirectory: path