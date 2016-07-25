stream protocol
testNextIntoClose
	"Ensure that #next:into: will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1) 
		equals: 'A line of text'.
