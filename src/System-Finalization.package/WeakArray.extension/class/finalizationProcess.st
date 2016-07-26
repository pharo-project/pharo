*System-Finalization
finalizationProcess
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The VM signals FinalizationSemaphore whenever
	 the queue is non-empty.  This process loops, waiting on the semaphore, fetches the first element
	 of the queue and then spawns a process at a higher priority to acually send the mourn messages.
	 If an error occurs in the higher priority mourn loop process then this process will simply spawn
	 another process, hence ensuring that errors in finalization methods don't break finalization.

	 In addition this process also runs the old finalization scheme, supporting clients of the older,
	 WeakRegistry based scheme.  Hopefully this will go away when all cleints have moved over."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue: [FinalizationSemaphore wait; initSignals.
	 "Support the old scheme until things have changed over..."
	 self doOldFinalization.
	 [firstMourner := self primitiveFetchMourner.
	  firstMourner notNil] whileTrue:
		[[throttle signal.
		  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1.
		 throttle wait]]