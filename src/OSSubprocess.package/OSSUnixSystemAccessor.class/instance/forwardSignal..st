USING FROM OSPROCESS
forwardSignal: signalNumber 
	"Set a signal handler in the VM which will signal a Smalltalk semaphore 
	at semaphoreIndex whenever an external signal signalNumber is received.
	Answer a new Semaphore, or nil if unable to set the handler (possibly
	because it has already been set). A Smalltalk process can wait on the
	Semaphore, and take action when a signal is detected. See man(7) signal
	for signal number definitions on your unix system."
	
	| sema index |
	sema := Semaphore new.
	index := Smalltalk registerExternalObject: sema.
	(self primForwardSignal: signalNumber toSemaphore: index)
		ifNil:
			[Smalltalk unregisterExternalObject: sema.
			^ nil].
	^ sema