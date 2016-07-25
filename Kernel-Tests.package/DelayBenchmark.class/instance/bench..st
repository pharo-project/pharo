benchmarks
bench: aDelaySchedulerClass
	| schedulerClassToRestore |
	
	self assert: (aDelaySchedulerClass inheritsFrom: DelayScheduler).
	schedulerClassToRestore := Delay delaySchedulerClass.
	
	[	Delay delaySchedulerClass: aDelaySchedulerClass.
		self bench.
	] ensure: [ Delay delaySchedulerClass: schedulerClassToRestore].