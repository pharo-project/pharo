USING FROM OSPROCESS
primForwardSignal: signalNumber toSemaphore: semaphoreIndex
	"Set a signal handler in the VM which will signal a Smalltalk semaphore at
	semaphoreIndex whenever an external signal sigNum is received. Answer the
	prior value of the signal handler. If semaphoreIndex is zero, the handler is
	unregistered, and the VM returns to its default behavior for handling that
	signal.

	The Smalltalk semaphore is expected to be kept at the same index location
	indefinitely during the lifetime of a Squeak session. If that is not the case, the
	handler must be unregistered prior to unregistering the Smalltalk semaphore."

	<primitive: 'primitiveForwardSignalToSemaphore' module: 'UnixOSProcessPlugin'>
	^ nil
