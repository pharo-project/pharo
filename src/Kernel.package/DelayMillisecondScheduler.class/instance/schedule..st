low priority processes
schedule: aDelay
	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	accessProtect critical: [
		scheduledDelay := aDelay.
		timingSemaphore signal. "#handleTimerEvent: sets scheduledDelay:=nil"
		].
	