testing
isDataAvailable
	"It the inbuffer is empty, we check the socket for data.
	If it claims to have data available to read, we try to read
	some once and recursively call this method again.
	If something really was available it is now in the inBuffer.
	This is because there has been spurious
	dataAvailable when there really is no data to get."
 
	self isInBufferEmpty ifFalse: [^true].
	^socket dataAvailable
		ifFalse: [false]
		ifTrue: [self receiveDataIfAvailable; isDataAvailable]