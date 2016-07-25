primitives
primSocketLocalPort: socketID
	"Return the local port for this socket, or zero if no port has yet been assigned."

	<primitive: 'primitiveSocketLocalPort' module: 'SocketPlugin'>
	self primitiveFailed
