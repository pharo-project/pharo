primitives
primitiveSSL: sslHandle decrypt: srcbuf startingAt: start count: length into: dstbuf
	"Primitive. Takes incoming data for decryption and continues to decrypt data.
	Returns the number of bytes produced in the output"

	<primitive: 'primitiveDecrypt' module: 'SqueakSSL'>
	
	^ self primitiveFailed