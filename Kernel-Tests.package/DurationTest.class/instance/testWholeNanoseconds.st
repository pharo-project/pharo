tests
testWholeNanoseconds
	self assert: 0 seconds wholeNanoseconds equals: 0.
	self assert: 1 second wholeNanoseconds equals: 0.
	self assert: 1002003004 nanoSeconds wholeNanoseconds equals: 4