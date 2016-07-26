queries
sendDone
	"Return true if the most recent send operation on this socket has completed."

	socketHandle == nil ifTrue: [^ false].
	^ self primSocketSendDone: socketHandle
