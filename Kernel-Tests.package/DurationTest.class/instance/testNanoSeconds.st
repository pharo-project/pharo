tests
testNanoSeconds
	self assert: aDuration nanoSeconds = 5.
	self assert: (Duration nanoSeconds: 5) nanoSeconds = 5.	