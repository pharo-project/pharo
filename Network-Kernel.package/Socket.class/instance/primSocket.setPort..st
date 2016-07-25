primitives
primSocket: socketID setPort: port
	"Set the local port associated with a UDP socket.
	Note: this primitive is overloaded.  The primitive will not fail on a TCP socket, but
	the effects will not be what was desired.  Best solution would be to split Socket into
	two subclasses, TCPSocket and UDPSocket."

	<primitive: 'primitiveSocketListenWithOrWithoutBacklog' module: 'SocketPlugin'>
	self primitiveFailed
