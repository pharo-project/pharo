private
critical: aBlock

	lock ifNil: [
		lock := Semaphore forMutualExclusion ].

	^ lock critical: aBlock