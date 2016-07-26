testing
testSkipLecture
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file nextPut: 3.
	file close.
	
	file := self fileStreamForFileNamed: 'testFile'.
	file skip: 2.
	self assert: file next equals: 3.