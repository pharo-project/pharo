tests
testExecuteWithoutDelay

	self count: 0.
	deferrer := OmDeferrer 
		send: #increase
		to: self
		after: 0 milliSeconds.

	deferrer schedule.

	"Just to document it needs an interruption"
	self assert: self count equals: 0.
	Processor yield.	
	self assert: self count equals: 1.