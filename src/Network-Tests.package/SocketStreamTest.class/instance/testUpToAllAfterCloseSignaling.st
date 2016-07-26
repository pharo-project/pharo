stream protocol
testUpToAllAfterCloseSignaling
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upToAll: String crlf] raise: ConnectionClosed.
