primitives
primSocket: socketID receiveDataInto: aStringOrByteArray startingAt: startIndex count: count
	"Receive data from the given socket into the given array starting at the given index. Return the number of bytes read or zero if no data is available."

	<primitive: 'primitiveSocketReceiveDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed
