primitives
primitiveSSL: sslHandle getStringProperty: propID
	"Primitive. Returns a string property from an SSL session"

	<primitive: 'primitiveGetStringProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed