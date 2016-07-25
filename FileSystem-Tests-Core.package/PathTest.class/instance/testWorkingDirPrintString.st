tests
testWorkingDirPrintString
	| path actual |
	path := Path workingDirectory. 
	actual := path printString.
	self assert: actual equals: 'Path workingDirectory'