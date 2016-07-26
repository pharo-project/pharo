stream protocol
testUpToAllAfterCloseNonSignaling
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upToAll: String crlf.
	self assert: resp = 'A line of text'.