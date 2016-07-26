snapshotting
restoreResumptionTimes
	"Private! Restore the resumption times of all scheduled Delays after a snapshot or clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| newBaseTime |
	newBaseTime := Time millisecondClockValue.
	suspendedDelays do: [ :delay | 
		self adjust: delay resumptionTimeOldBase: 0 newBase: newBaseTime ].
	activeDelay == nil ifFalse: [
		self adjust: activeDelay resumptionTimeOldBase: 0 newBase: newBaseTime ].
