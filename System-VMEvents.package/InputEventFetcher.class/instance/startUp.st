initialize-release
startUp
	inputSemaphore := Semaphore new.
	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	inputSemaphore consumeAllSignals.
	self installEventLoop