primitives
primSocketRemotePort: socketID
	"Return the remote port for this socket, or zero if no connection has been made."

	<primitive: 'primitiveSocketRemotePort' module: 'SocketPlugin'>
	self primitiveFailed
