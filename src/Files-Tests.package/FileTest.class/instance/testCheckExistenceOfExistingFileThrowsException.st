tests
testCheckExistenceOfExistingFileThrowsException

	| file |
	file := File named: 'asd.txt'.
	file writeStream.
	self should: [file checkDoesNotExist] raise: FileAlreadyExistsException.