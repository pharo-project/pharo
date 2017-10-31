tests
testDirectoryExists

	| dir |
	dir := Directory named: 'testtesttest'.
	dir create.
	self assert: dir exists.
	dir delete.