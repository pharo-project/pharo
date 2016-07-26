primitives
primSocketError: socketID
	"Return an integer encoding the most recent error on this socket. Zero means no error."

	<primitive: 'primitiveSocketError' module: 'SocketPlugin'>
	self primitiveFailed
