tests
testSingleTiming
	| timeBefore |
	timeBefore := DateAndTime now.
	aStopwatch activate.
	aDelay wait.
	aStopwatch suspend.
	self assert: aStopwatch timespans size = 1. 
	self assert: aStopwatch timespans first asDateAndTime >= timeBefore. 
	self assert: aStopwatch timespans first asDateAndTime <= aStopwatch end.
