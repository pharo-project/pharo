stream protocol
testUpToEndCloseNonSignaling
	"Ensure that #upToEnd will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd)
		equals: 'A line of text'.
