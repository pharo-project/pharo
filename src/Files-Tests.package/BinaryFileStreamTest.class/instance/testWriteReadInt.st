testing
testWriteReadInt
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: 1.