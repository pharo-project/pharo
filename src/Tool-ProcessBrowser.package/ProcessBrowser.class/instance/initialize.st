initialization
initialize
	super initialize.
	self class registerWellKnownProcesses.
	methodText := ''.
	stackListIndex := 0.
	searchString := ''.
	lastUpdate := 0.
	textModel :=  RubScrolledTextModel new interactionModel: self; yourself.
	startedCPUWatcher := CPUWatcher cpuWatcherEnabled and: [ self startCPUWatcher ].
	self updateProcessList; processListIndex: 1