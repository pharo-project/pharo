primitives
primitiveSSLDestroy: sslHandle
	"Primitive. Destroys the SSL session handle in the VM plugin"

	<primitive: 'primitiveDestroy' module: 'SqueakSSL'>
	
	^ self primitiveFailed