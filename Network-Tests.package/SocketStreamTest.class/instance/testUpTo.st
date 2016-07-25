stream protocol
testUpTo
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll:'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) = 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: Character cr) = 'with more text'.
