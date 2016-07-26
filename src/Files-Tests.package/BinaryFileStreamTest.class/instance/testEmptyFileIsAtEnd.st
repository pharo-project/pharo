testing
testEmptyFileIsAtEnd
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file atEnd