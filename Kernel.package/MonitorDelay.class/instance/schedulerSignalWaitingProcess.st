private
schedulerSignalWaitingProcess
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	monitor signalLock: delaySemaphore inQueue: queue.
