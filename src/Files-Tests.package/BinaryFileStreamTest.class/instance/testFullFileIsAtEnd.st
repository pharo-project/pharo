testing
testFullFileIsAtEnd
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	self assert: file atEnd.