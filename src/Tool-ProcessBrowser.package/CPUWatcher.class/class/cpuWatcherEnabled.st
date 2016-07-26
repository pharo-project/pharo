setting
cpuWatcherEnabled
	^ CpuWatcherEnabled ifNil: [CpuWatcherEnabled := false]