stream protocol
testUpToAfterCloseSignaling
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upTo: Character cr] raise: ConnectionClosed.
