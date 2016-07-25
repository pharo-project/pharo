system attributes
supportsQueueingFinalization: aBoolean
	"Determine whether the VM queues individual weak arrays for finalization, instead
	 of signalling the finalization semaphore once for all arrays and having the
	 WeakRegistry mechanism finalize all weak arrays, whether they need to or not.
	 This flag persists across snapshots, stored in the image header."
	"SmalltalkImage current supportsQueueingFinalization: true"

	self vm parameterAt: 48 put: ((self vm parameterAt: 48) bitClear: 16) + (aBoolean ifTrue: [16] ifFalse: [0])