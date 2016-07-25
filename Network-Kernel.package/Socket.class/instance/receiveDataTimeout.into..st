receiving
receiveDataTimeout: timeout into: aStringOrByteArray 
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once."

	^self receiveDataTimeout: timeout into: aStringOrByteArray startingAt: 1