stream protocol
testUpToWithByteArray
	"Tests correct behavior of #upTo:"

	|byteArray|
	
	byteArray := ('A line of text', String cr, 'with more text') asByteArray.
	clientStream binary.
	serverStream binary.
	clientStream nextPutAll: byteArray; flush.
	self assert: (serverStream upTo: Character cr) asString = 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPut: Character cr; flush] fork.
	self assert: (serverStream upTo: Character cr) asString = 'with more text'.
