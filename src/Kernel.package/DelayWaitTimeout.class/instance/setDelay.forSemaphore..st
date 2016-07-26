private
setDelay: anInteger forSemaphore: aSemaphore
	super setDelay: anInteger forSemaphore: aSemaphore.
	process := Processor activeProcess.
	expired := false.