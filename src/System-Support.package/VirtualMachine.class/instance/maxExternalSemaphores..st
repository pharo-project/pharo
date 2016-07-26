attributes
maxExternalSemaphores: aSize
	"This method should never be called as result of normal program
    execution. If it is however, fork off the growth to keep new
    Semaphores flowing for other users, but raise an error in this thread
    indicating that something needs to change to avoid potentially
    unhandled interrupts."

	self maxExternalSemaphores ifNil: [ ^ 0 ].
	
	[ self maxExternalSemaphoresSilently: aSize.
	self
		crTrace: 'WARNING: Had to increase size of semaphore signal handling table due to many external objects concurrently in use';
		crTrace: 'You should increase this size at startup using #maxExternalSemaphoresSilently:';
		crTrace: 'Current table size: ' , self maxExternalSemaphores printString ] forkAt: Processor userBackgroundPriority.
	
	self error: 'Not enough space for external objects, set a larger size at startup!'.
	^ self maxExternalSemaphores