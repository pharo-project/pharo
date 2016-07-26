printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(.
	stream nextPutAll: endianness; nextPutAll: ' endian'.
	stream nextPut: $)