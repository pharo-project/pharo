tests
testSemaphoreNoTimeout
	| sem process |
	sem := Semaphore new.
	[ | delay |
		process := [ delay := Delay timeoutSemaphore: sem afterMSecs: 1000. sem wait] newProcess.
		process priority: Processor highIOPriority.
		process resume.
		delay unschedule.
		self deny: process isTerminated.
	] ensure: [sem signal].
	self assert: process isTerminated.
