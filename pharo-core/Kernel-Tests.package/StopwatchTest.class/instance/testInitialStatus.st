tests
testInitialStatus
	self assert: aStopwatch isSuspended.
	self deny: aStopwatch isActive.
	self assert: aStopwatch duration = 0 seconds