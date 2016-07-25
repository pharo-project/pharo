testing
isConnected
	"Returns true if the SSL handshake has been completed"

	^ self sslState = 3