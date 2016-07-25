primitives
primSocketConnectionStatus: socketID
	"Return an integer reflecting the connection status of this socket. For a list of possible values, see the comment in the 'initialize' method of this class. If the primitive fails, return a status indicating that the socket handle is no longer valid, perhaps because the Pharo image was saved and restored since the socket was created. (Sockets do not survive snapshots.)"

	<primitive: 'primitiveSocketConnectionStatus' module: 'SocketPlugin'>
	^ InvalidSocket
