receiving
receiveAvailableDataInto: buffer
	"Receive all available data into the given buffer and return the number of bytes received.
	Note the given buffer may be only partially filled by the received data.
	Do not wait for data."

	^self receiveAvailableDataInto: buffer startingAt: 1