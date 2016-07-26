primitives
primAcceptFrom: aHandle receiveBufferSize: rcvBufSize sendBufSize: sndBufSize semaIndex: semaIndex
	"Create and return a new socket handle based on accepting the connection from the given listening socket"

	<primitive: 'primitiveSocketAccept' module: 'SocketPlugin'>

	^ self primitiveFailed