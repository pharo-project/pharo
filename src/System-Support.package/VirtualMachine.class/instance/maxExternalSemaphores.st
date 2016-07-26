attributes
maxExternalSemaphores
	"The size of array in some VM's where external signals for
	semaphores in externalObjects are handled.
	Essentially, if a semaphore is registered in externalObjects outside
	its bounds, they will not be signalled."
	^ [self parameterAt: 49]
		on: PrimitiveFailed
		do: [:ex | ex return: nil]