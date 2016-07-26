testing
testSpaces
	self 
		assert: 'file:///foo%20bar/test.st' asZnUrl asFileReference 
		equals: FileLocator root / 'foo bar' / 'test.st'.
	self 
		assert: (FileLocator root / 'foo bar' / 'test.st') asZnUrl 
		equals: 'file:///foo%20bar/test.st' asZnUrl