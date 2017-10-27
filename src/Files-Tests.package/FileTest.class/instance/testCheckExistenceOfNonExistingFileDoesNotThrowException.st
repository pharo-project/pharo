tests
testCheckExistenceOfNonExistingFileDoesNotThrowException

	| file |
	file := File named: 'asd.txt'.
	self shouldnt: [file checkDoesNotExist] raise: Error.