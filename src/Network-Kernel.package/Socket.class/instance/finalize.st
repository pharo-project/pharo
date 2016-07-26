finalization
finalize
	self primSocketDestroyGently: socketHandle.
	Smalltalk unregisterExternalObject: semaphore.
	Smalltalk unregisterExternalObject: readSemaphore.
	Smalltalk unregisterExternalObject: writeSemaphore.
