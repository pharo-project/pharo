tests
testCritical
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled