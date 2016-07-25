tests
testDurationLessThanMaxTicks
	"In #handleTimerEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in milliseconds"
	| duration nowTick nextTick expectedTick scheduler |
	duration := 789 milliSeconds.  "simply less than maxTicks"
	nowTick := 100 milliSeconds. 
	expectedTick := nowTick + duration.

	scheduler := DelayMillisecondScheduler new.
	scheduler testCaseSupportSetActiveDelayStartTime: 0.
	scheduler schedule: duration asDelay. 
	nextTick := scheduler handleTimerEvent: nowTick asMilliSeconds.

	self assert: nextTick 
			equals: expectedTick asMilliSeconds.		
	self assert: scheduler testCaseSupportActiveDelay millisecondDelayDuration 
			equals: duration asMilliSeconds.
	