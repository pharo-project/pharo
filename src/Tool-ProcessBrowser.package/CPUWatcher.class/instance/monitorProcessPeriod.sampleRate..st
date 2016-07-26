startup-shutdown
monitorProcessPeriod: secs sampleRate: msecs
	self stopMonitoring.

	watcher := [ [ | promise |
		promise := Processor tallyCPUUsageFor: secs every: msecs.
		tally := promise value.
		promise := nil.
		self findThePig.
	] repeat ] forkAt: Processor highestPriority.
	Processor yield 