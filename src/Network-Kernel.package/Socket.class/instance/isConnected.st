queries
isConnected
	"Return true if this socket is connected."

	socketHandle == nil ifTrue: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == Connected
