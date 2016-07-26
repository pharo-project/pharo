tests
testMultipleTimings
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	self assert: aStopwatch timespans size = 2. 
	self assert: aStopwatch timespans first asDateAndTime <= 
					aStopwatch timespans last asDateAndTime.
