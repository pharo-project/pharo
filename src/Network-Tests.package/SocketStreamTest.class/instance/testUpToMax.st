stream protocol
testUpToMax
	"Tests correct behavior of #upToAll:max:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upTo: Character cr limit: 5) = 'A line of text'.