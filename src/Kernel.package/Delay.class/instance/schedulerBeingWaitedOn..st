private - scheduler
schedulerBeingWaitedOn: aBoolean
	"Indicate whether this delay is currently scheduled, i.e. being waited on"
	
	beingWaitedOn := aBoolean

	"Important! Must only be called from the single timing priority process, e.g...
		DelayScheduler>>scheduleDelay: 
		DelayScheduler>>unscheduleDelay: "
