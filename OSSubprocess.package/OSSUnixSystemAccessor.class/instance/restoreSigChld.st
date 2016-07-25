USING FROM OSPROCESS
restoreSigChld
	"Unset a SIGCHLD signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	^ self restoreSignal: self primSigChldNumber
