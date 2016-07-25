system attributes
supportsQueueingFinalization
	"Determine whether the VM queues individual weak arrays for finalization, instead
	 of signalling the finalization semaphore once for all arrays and having the
	 WeakRegistry mechanism finalize all weak arrays, whether they need to or not.
	 This flag persists across snapshots, stored in the image header."
	"SmalltalkImage current supportsQueueingFinalization"

	^ ((self vm parameterAt: 48) bitAnd: 16) = 16