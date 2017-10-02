tests
testForMilliseconds
	| scheduler delay preTick postTick actualDuration |
	scheduler := self delaySchedulerClass new.
	delay := Delay forMilliseconds: 1000.
	scheduler schedule: delay.
	
	preTick := Time millisecondClockValue.
	[ 	scheduler handleTimerEvent: Time microsecondClockValue.
		Processor yield.
		delay delaySemaphore isSignaled
	] whileFalse.
	postTick := Time millisecondClockValue.
	
	actualDuration := postTick - preTick.
	self assert: actualDuration >= 950.
	self assert: actualDuration <= 1050.