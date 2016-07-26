initialization
windowIsClosing
	startedCPUWatcher ifTrue: [ CPUWatcher stopMonitoring ]