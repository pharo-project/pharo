tests
testDirectoryDoesNotExistIfNotCreated

	| dir |
	dir := Directory named: 'testtesttest'.
	self deny: dir exists.