testing
testPeekDoesNotAdvanceTheStream
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file nextPut: 3.
	file close.
	
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file position equals: 0.
	self assert: file peek equals: file peek.
	self assert: file position equals: 0.