tests
testAsNanoSeconds
	self assert: aDateAndTime asNanoSeconds =  aDuration asNanoSeconds.
	self assert: aDateAndTime asNanoSeconds = 48780000000000
	
