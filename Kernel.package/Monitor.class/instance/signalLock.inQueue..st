private
signalLock: aSemaphore inQueue: anOrderedCollection
	queuesMutex critical: [
		aSemaphore signal.
		anOrderedCollection remove: aSemaphore ifAbsent: [].
	].