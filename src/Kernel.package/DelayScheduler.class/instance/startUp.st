snapshotting
startUp
	"Restart active delay, if any, when resuming a snapshot."

	"Compare to false since it can be nil"
	(delaySuspended = false) 
		ifTrue:[ ^self error: 'Trying to activate Delay twice' ].
	delaySuspended := false.
	
	activeDelayStartTime := Time millisecondClockValue.
	self restoreResumptionTimes.
	accessProtect signal.
