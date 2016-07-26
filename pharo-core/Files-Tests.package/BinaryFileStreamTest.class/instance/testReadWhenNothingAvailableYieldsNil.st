testing
testReadWhenNothingAvailableYieldsNil
	
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: nil.