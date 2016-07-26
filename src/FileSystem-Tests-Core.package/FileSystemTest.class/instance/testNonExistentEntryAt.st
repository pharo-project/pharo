tests
testNonExistentEntryAt
	| path1 path2  |
	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	filesystem createDirectory: path1.

	self shouldnt: [ filesystem entryAt: path1 ] raise: FileDoesNotExist.
	self should: [ filesystem entryAt: path2 ] raise: FileDoesNotExist