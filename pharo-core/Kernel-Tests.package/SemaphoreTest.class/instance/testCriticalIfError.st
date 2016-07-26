tests
testCriticalIfError
	| lock |
	lock := Semaphore forMutualExclusion.
	[lock critical: [self criticalError ifError:[]]] forkAt: Processor userInterruptPriority.
	self assert: lock isSignaled