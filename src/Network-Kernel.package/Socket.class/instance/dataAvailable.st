queries
dataAvailable
	"Return true if this socket has unread received data."

	socketHandle == nil ifTrue: [^ false].
	^ self primSocketReceiveDataAvailable: socketHandle
