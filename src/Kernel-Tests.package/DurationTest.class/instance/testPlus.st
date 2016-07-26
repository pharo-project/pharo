tests
testPlus
	self assert: (aDuration + 0 hours) = aDuration.
	self assert: (aDuration + aDuration) = (Duration days: 2 hours: 4 minutes: 6 seconds: 8 nanoSeconds: 10). 