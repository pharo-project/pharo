queries
isThisEndClosed
	"Return true if this socket had the this end closed."

	socketHandle == nil ifTrue: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == ThisEndClosed
