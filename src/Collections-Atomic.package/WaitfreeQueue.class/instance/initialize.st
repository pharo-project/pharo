initialization
initialize
	dummy := AtomicQueueItem new.
	dummy object: dummy.
	tail := dummy.
