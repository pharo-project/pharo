class initialization
initialize
	"Delay initialize"
	
	"Get the current timing semaphore, or install it if there is none"
	timingSemaphore := (Smalltalk specialObjectsArray at: 30) ifNil: [ 
		Smalltalk specialObjectsArray at: 30 put: Semaphore new ].
	
	delaySuspended := true.
	accessProtect := Semaphore forMutualExclusion.

	"Case 14353 - The following is moving here from #startTimerEventLoop to help with tests. 
	Its also left in its original location for now, to be cleaned up later."
	suspendedDelays := Heap 
			sortBlock: [ :delay1 :delay2 | delay1 schedulerResumptionTime <= delay2 schedulerResumptionTime ].
