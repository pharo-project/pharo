child watching
sigChldSemaphore
	"Answer the value of sigChldSemaphore.
	If nil, then register and create a semaphore to be signaled upon SIGCHLD.
	See #forwardSigChld for more details. "

	^ sigChldSemaphore ifNil: [sigChldSemaphore := self systemAccessor forwardSigChld].
