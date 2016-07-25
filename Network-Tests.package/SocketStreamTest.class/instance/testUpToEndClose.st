stream protocol
testUpToEndClose
	"Ensure that #upToEnd will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd) 
		equals: 'A line of text'.
