tests
testAsNanoSeconds
	self assert: (Duration nanoSeconds: 1)  asNanoSeconds = 1.
	self assert: (Duration seconds: 1)  asNanoSeconds = 1000000000.	
	self assert: aDuration   asNanoSeconds = 93784000000005.