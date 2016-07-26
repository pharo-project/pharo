control
receiveData: nBytes
	"Keep reading the socket until we have nBytes
	in the inBuffer or we reach the end. This method
	does not return data, but can be used to make sure
	data has been read into the buffer from the Socket
	before actually reading it from the FastSocketStream.
	Mainly used internally. We could also adjust the buffer
	to the expected amount of data and avoiding several
	incremental grow operations.

	NOTE: This method doesn't honor timeouts if shouldSignal
	is false! And frankly, I am not sure how to handle that
	case or if I care - I think we should always signal."

	[self isConnected and: [nBytes > self inBufferSize]]
		whileTrue: [self receiveData]