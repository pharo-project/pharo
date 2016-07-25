primitives
primSocket: socketID sendData: aStringOrByteArray startIndex: startIndex count: count
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed."

	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created."

	<primitive: 'primitiveSocketSendDataBufCount' module: 'SocketPlugin'>
	^ self
		errorSending: aStringOrByteArray
		startingAt: startIndex
		count: count
		toHost: self remoteAddress
		port: self remotePort