primitives
primitiveSSL: sslHandle encrypt: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Encrypts the incoming buffer into the result buffer.
	Returns the number of bytes produced as a result"

	<primitive: 'primitiveEncrypt' module: 'SqueakSSL'>
	
	^ self primitiveFailed