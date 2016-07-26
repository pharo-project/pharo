stream protocol
testUpToAfterCloseNonSignaling
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upTo: Character cr.
	self assert: resp = 'A line of text'.