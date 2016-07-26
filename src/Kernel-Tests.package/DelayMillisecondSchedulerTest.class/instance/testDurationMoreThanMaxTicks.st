tests
testDurationMoreThanMaxTicks
	"In #handleTimerEvent the maximum tick is hardcoded as 1000 milliseconds.
	 Test when delay duration is less than this.
		- duration is in milliseconds.
		- ticks are in milliseconds"
	| duration maxTicks  nowTick nextTick expectedTick scheduler |
	duration := 1234 milliSeconds. "simply more than maxTicks"
	maxTicks := 1000 milliSeconds. 
	nowTick := 100 milliSeconds.
	expectedTick := nowTick + maxTicks.

	scheduler := DelayMillisecondScheduler new.
	scheduler testCaseSupportSetActiveDelayStartTime: 0.
	scheduler schedule: duration asDelay. 
	nextTick := scheduler handleTimerEvent: nowTick asMilliSeconds.

	self assert: nextTick 
			equals: expectedTick asMilliSeconds.		
	self assert: scheduler testCaseSupportActiveDelay millisecondDelayDuration 
			equals: duration asMilliSeconds.
	