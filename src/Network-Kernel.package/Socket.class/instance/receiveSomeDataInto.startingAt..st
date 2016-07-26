receiving
receiveSomeDataInto: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	^ self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size-aNumber+1
