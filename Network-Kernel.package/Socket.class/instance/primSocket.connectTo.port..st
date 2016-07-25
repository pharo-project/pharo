primitives
primSocket: socketID connectTo: hostAddress port: port
	"Attempt to establish a connection to the given port of the given host. This is an asynchronous call; query the socket status to discover if and when the connection is actually completed."

	<primitive: 'primitiveSocketConnectToPort' module: 'SocketPlugin'>
	
	self primitiveFailed
