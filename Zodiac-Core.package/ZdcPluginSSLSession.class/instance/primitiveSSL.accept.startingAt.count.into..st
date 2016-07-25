primitives
primitiveSSL: sslHandle accept: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Starts or continues a server handshake using the provided data.
	Will eventually produce output to be sent to the client.
	Returns:
		> 0   Number of bytes to be sent to the server
		= 0   Success. The connection is established
		= -1  More input is required
		< -1  Other errors"
		
	<primitive: 'primitiveAccept' module: 'SqueakSSL'>
	
	^ self primitiveFailed