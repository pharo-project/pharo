tests
testWholeMilliseconds
	self assert: 0 seconds wholeMilliseconds equals: 0.
	self assert: 1 second wholeMilliseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeMilliseconds equals: 2