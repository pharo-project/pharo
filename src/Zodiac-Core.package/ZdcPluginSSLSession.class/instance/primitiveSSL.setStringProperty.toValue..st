primitives
primitiveSSL: sslHandle setStringProperty: propID toValue: aString
	"Primitive. Sets a string property in an SSL session"

	<primitive: 'primitiveSetStringProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed