testing
testFileWithSomeBytesSizeIsNotZero
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	self assert: file position equals: 1.
	file nextPutAll: #[1 2 3].
	self assert: file position equals: 4.
	file nextPut: 1.
	self assert: file position equals: 5.