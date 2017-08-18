accessing
serverName: serverName
	"Set the Server Name Indication (SNI) to serverName, 
	the fully qualified domain name of the host to connect to.
	This should be set by a client before #connect is attempted."
	
	^ self primitiveSSL: handle setStringProperty: 2 toValue: (serverName ifNil: [''])