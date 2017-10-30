streaming
critical: block
	"Execute block making sure only one thread accesses the receiver"

	^ accessSemaphore critical: block 