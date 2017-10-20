initialization
initialize
	super initialize.
	scheduledDelayIsNil := Semaphore new.
	finishedDelayIsNil := Semaphore new.