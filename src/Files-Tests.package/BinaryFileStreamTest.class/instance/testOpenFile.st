testing
testOpenFile
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	
	self assert: (File named: 'testFile') exists