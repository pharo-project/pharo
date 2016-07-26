*System-Finalization
restartFinalizationProcess
	"kill any old process, just in case"
	FinalizationProcess
		ifNotNil: [FinalizationProcess terminate.
			FinalizationProcess := nil].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	
	FinalizationProcess := [self finalizationProcess]
		forkAt: Processor userInterruptPriority.
	FinalizationProcess name: 'WeakArray Finalization Process'.