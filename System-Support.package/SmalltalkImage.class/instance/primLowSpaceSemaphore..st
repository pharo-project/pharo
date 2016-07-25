memory space
primLowSpaceSemaphore: aSemaphore
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed