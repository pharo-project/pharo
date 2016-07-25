primitives
primSocket: socketID listenOn: port
	"Listen for a connection on the given port. This is an asynchronous call; query the socket status to discover if and when the connection is actually completed."

	<primitive: 'primitiveSocketListenWithOrWithoutBacklog' module: 'SocketPlugin'>
	self primitiveFailed
