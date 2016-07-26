USING FROM OSPROCESS
forwardSigChld
	"Set a signal handler for SIGCHLD. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	^ self forwardSignal: self primSigChldNumber
