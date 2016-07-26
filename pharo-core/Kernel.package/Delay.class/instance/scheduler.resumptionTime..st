private - scheduler
scheduler: aDelayScheduler resumptionTime: milliOrMicroResumptionTime
	"The milli/micro semantics of schedulerResumptionTime varies depending on which 
	scheduler is running, so we record it such that later printOn: can interpret it
	using #millisecondsToGo"
	scheduler := aDelayScheduler.
	schedulerResumptionTime := milliOrMicroResumptionTime.