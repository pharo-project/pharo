tests
testRootPrintString
	| path actual |
	path := Path root.
	actual := path printString.
	self assert: actual equals: 'Path root'