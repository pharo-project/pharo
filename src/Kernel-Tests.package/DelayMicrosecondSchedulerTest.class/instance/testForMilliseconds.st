tests
testForMilliseconds
	| scheduler delay preTick postTick actualDuration |
	scheduler := DelayMicrosecondScheduler new.
	delay := Delay forMilliseconds: 100.
	scheduler schedule: delay.
	
	preTick := Time millisecondClockValue.
	[ 	scheduler handleTimerEvent: Time microsecondClockValue.
		Processor yield.
		delay delaySemaphore isSignaled
	] whileFalse.
	postTick := Time millisecondClockValue.
	
	actualDuration := postTick - preTick.
	self assert: actualDuration >= 95.
	self assert: actualDuration <= 105.