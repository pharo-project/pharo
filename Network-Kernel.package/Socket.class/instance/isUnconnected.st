queries
isUnconnected
	"Return true if this socket's state is Unconnected."

	socketHandle == nil ifTrue: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == Unconnected
