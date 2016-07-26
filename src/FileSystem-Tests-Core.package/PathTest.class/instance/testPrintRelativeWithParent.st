tests
testPrintRelativeWithParent
	| path |
	path := Path parent / 'foo'.
	self assert: (path printWithDelimiter: $/) equals: '../foo'