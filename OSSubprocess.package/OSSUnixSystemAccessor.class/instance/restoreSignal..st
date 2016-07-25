USING FROM OSPROCESS
restoreSignal: signalNumber 
	"Unset a signal handler and unregister the Smalltalk semaphore. Answer
	the unregistered Semaphore, or nil if unable to restore the signal (possibly
	because no handler had been set)."

	| semaphoreIndex sema |
	semaphoreIndex := self primSemaIndexFor: signalNumber.
	semaphoreIndex ifNotNil:
		[sema := Smalltalk externalObjects at: semaphoreIndex ifAbsent: [].
		sema ifNotNil:
			[self primForwardSignal: signalNumber toSemaphore: nil.
			Smalltalk unregisterExternalObject: sema]].
	^ sema
