connection open/close
connectNonBlockingTo: hostAddress port: port
	"Initiate a connection to the given port at the given host address. This operation will return immediately; follow it with waitForConnectionUntil: to wait until the connection is established."

	| status |
	self initializeNetwork.
	status := self primSocketConnectionStatus: socketHandle.
	(status == Unconnected)
		ifFalse: [InvalidSocketStatusException signal: 'Socket status must Unconnected before opening a new connection'].

	self primSocket: socketHandle connectTo: hostAddress port: port