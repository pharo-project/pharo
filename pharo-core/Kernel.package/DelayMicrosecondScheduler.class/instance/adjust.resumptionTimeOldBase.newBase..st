snapshotting
adjust: aDelay resumptionTimeOldBase: oldBaseTime newBase: newBaseTime
	"Used to adjust resumption times after a snapshot."

	aDelay schedulerResumptionTime: newBaseTime + ( aDelay schedulerResumptionTime - oldBaseTime).

	"Important! Must only be called from the single timing priority process, e.g...
		DelayScheduler>>saveResumptionTimes 
		DelayScheduler>>restoreResumptionTimes "