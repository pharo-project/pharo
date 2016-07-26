background process
idleProcess
	"A default background process which is invisible."

	[true] whileTrue:
		[self relinquishProcessorForMicroseconds: 1000]