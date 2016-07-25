queries
isWaitingForConnection
	"Return true if this socket is waiting for a connection."

	socketHandle == nil ifTrue: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == WaitingForConnection
