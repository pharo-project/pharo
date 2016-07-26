initialize-release
terminateProcess
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].