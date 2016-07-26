primitives
primSocketDestroyGently: socketID
	"Release the resources associated with this socket. If a connection is open, it is aborted.
	Do not fail if the receiver is already closed."

	<primitive: 'primitiveSocketDestroy' module: 'SocketPlugin'>
