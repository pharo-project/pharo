initialize
initialize
	super initialize.
	random := Random new.
	lock := Semaphore forMutualExclusion.
	node := self computeNodeIdentifier.
	counter := self nextRandom16