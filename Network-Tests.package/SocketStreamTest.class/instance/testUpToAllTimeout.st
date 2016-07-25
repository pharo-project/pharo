stream protocol
testUpToAllTimeout
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [serverStream upToAll: String crlf] raise: ConnectionTimedOut.
