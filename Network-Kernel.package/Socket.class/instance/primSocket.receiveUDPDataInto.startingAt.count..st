primitives
primSocket: socketID receiveUDPDataInto: aStringOrByteArray startingAt: startIndex count: count
	"Receive data from the given socket into the given array starting at the given index. 
	Return an Array containing the amount read, the host address byte array, the host port, and the more flag"

	<primitive: 'primitiveSocketReceiveUDPDataBufCount' module: 'SocketPlugin'>
	self primitiveFailed
