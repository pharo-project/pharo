stream protocol
testUpToAll
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text', String crlf, 'with more text'; flush.
	self assert: (serverStream upToAll: String crlf) = 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String crlf; flush] fork.
	self assert: (serverStream upToAll: String crlf) = 'with more text'.
