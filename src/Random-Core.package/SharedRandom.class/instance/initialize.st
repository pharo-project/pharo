initialization
initialize
	super initialize.
	mutex := Semaphore forMutualExclusion.