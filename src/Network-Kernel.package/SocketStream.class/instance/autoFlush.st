configuration
autoFlush
	"If autoFlush is enabled data will be sent through
	the socket (flushed) when the bufferSize is reached
	or the SocketStream is closed. Otherwise the user
	will have to send #flush manually.
	Close will always flush. Default is false."

	^autoFlush