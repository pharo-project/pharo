receiving
receiveDataInto: aStringOrByteArray
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	^self receiveDataInto: aStringOrByteArray startingAt: 1