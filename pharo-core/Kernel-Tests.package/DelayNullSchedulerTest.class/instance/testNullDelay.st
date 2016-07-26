tests
testNullDelay
	| scheduler delay preTick postTick actualDuration |
	scheduler := DelayNullScheduler new.
	delay := Delay forMilliseconds: 1000.
	
	preTick := Time millisecondClockValue.
	scheduler schedule: delay.  "The null #schedule: immediately signals the delaySemaphore..."
	delay delaySemaphore wait. "...so there is no wait"
	postTick := Time millisecondClockValue.	
		
	actualDuration := postTick - preTick.
	self assert: actualDuration <= 1 "this would have been near 1000 for a working delay".