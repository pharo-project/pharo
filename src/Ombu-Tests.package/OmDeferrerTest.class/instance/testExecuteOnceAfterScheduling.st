tests
testExecuteOnceAfterScheduling

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 250 milliSeconds.

	deferrer schedule.
	(Delay forMilliseconds: 350) wait.
	
	self assert: self count equals: 1.