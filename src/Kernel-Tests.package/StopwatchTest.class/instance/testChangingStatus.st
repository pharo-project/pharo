tests
testChangingStatus
	aStopwatch activate.
	self assert: aStopwatch isActive.
	self assert: aStopwatch timespans size = 1.
	aStopwatch suspend.
	self assert: aStopwatch isSuspended.
	self assert: aStopwatch timespans size = 1.
	aStopwatch activate.
	aStopwatch reActivate.
	self assert: aStopwatch isActive.
	self assert: aStopwatch timespans size = 3.
	aStopwatch reset.
	self assert: aStopwatch isSuspended.
	self assert: aStopwatch timespans size = 0.