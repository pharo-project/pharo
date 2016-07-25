receiving
receiveDataWithTimeout
	"Receive data into the given buffer and return the number of bytes received. 
	Note the given buffer may be only partially filled by the received data.
	Waits for data once.
	Either returns data or signals a time out or connection close."

	| buffer bytesRead |
	buffer := String new: 2000.
	bytesRead := self receiveDataWithTimeoutInto: buffer.
	^buffer copyFrom: 1 to: bytesRead