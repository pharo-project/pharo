receiving
receiveDataTimeout: timeout into: aStringOrByteArray startingAt: aNumber
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Wait for data once for the specified nr of seconds.  The answer may be 
	zero (indicating that there was no data available within the given timeout)."

	self waitForDataFor: timeout ifClosed: [] ifTimedOut: [].
	^self primSocket: socketHandle
		receiveDataInto: aStringOrByteArray
		startingAt: aNumber
		count: aStringOrByteArray size-aNumber+1
