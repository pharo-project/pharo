primitives
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema
	"See comment in primSocketCreateNetwork: with one semaIndex. However you should know that some implementations
	ignore the buffer size and this interface supports three semaphores,  one for open/close/listen and the other two for
	reading and writing"

	<primitive: 'primitiveSocketCreate3Semaphores' module: 'SocketPlugin'>
	^ self primitiveFailed