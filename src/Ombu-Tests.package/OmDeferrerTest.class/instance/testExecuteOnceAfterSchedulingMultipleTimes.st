tests
testExecuteOnceAfterSchedulingMultipleTimes

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 250 milliSeconds.

	3 timesRepeat: [	
		deferrer schedule.
		(Delay forMilliseconds: 50) wait ].
	
	(Delay forMilliseconds: 350) wait.
	
	self assert: self count equals: 1.