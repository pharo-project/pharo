tests
testDivide
	self assert: aDuration / aDuration = 1. 
	self assert: aDuration / 2 = (Duration days: 0 hours: 13 minutes: 1 seconds: 32 nanoSeconds: 2). 
	self assert: aDuration / (1/2) = (Duration days: 2 hours: 4 minutes: 6 seconds: 8 nanoSeconds: 10).
