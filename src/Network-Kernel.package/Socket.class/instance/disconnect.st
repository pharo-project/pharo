connection open/close
disconnect
	"Break this connection, no matter what state it is in. Data that has been sent but not received will be lost."

	self primSocketAbortConnection: socketHandle.
