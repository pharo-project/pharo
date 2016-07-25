primitives
primitiveSSL: sslHandle getIntProperty: propID
	"Primitive. Returns a string property from an SSL session"

	<primitive: 'primitiveGetIntProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed