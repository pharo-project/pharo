tests
testDirectoryDelete

	| dir |
	dir := Directory named: 'testtesttest'.
	dir create.
	dir delete.
	self deny: dir exists.