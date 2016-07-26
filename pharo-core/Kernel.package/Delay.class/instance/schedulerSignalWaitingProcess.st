private - scheduler
schedulerSignalWaitingProcess
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	delaySemaphore signal.

	"Important! Must only be called from the single timing priority process e.g...
		DelayScheduler>>handleEventTimer."