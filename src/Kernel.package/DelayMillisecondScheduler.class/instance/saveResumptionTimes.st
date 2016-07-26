snapshotting
saveResumptionTimes
	"Private! Record the resumption times of all Delays relative to a base time of zero. This is done prior to snapshotting or adjusting the resumption times after a clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| oldBaseTime |
	oldBaseTime := Time millisecondClockValue. 
	activeDelay == nil
		ifFalse: [
			oldBaseTime < activeDelayStartTime
				ifTrue: [ oldBaseTime := activeDelayStartTime ].  "clock rolled over"
			self adjust: activeDelay resumptionTimeOldBase: oldBaseTime newBase: 0 ].
		
	suspendedDelays do: [ :delay |
		self adjust: delay resumptionTimeOldBase: oldBaseTime newBase: 0 ].
