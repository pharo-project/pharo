testing
testWriteToClosedFileFails
	
	| fileStream |
	fileStream := self fileStreamForFileNamed: 'testFile'.
	fileStream close.
	self should: [ fileStream nextPut: 1 ] raise: Error.