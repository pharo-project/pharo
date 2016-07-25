USING FROM OSPROCESS
primSemaIndexFor: sigNum
	"Answer the registration index of the semaphore currently associated with the
	signal handler for sigNum."

	<primitive: 'primitiveSemaIndexFor' module: 'UnixOSProcessPlugin'>
	^ nil
