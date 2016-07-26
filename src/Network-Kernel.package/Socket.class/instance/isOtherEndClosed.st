queries
isOtherEndClosed
	"Return true if this socket had the other end closed."

	socketHandle == nil ifTrue: [^ false].
	^ (self primSocketConnectionStatus: socketHandle) == OtherEndClosed
