primitives
primitiveSSL: sslHandle setIntProperty: propID toValue: anInteger
	"Primitive. Sets an integer property in an SSL session"

	<primitive: 'primitiveSetIntProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed