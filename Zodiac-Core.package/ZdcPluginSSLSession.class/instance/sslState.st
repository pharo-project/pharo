accessing
sslState
	"Returns the current state of the SSL connection:
		0 - Unused
		1 - In accept handshake
		2 - In connect handshake
		3 - Connected"
		
	^ self primitiveSSL: handle getIntProperty: 2
		