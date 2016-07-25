testing
isConnected
	"Are we connected at the socket level ?"
	
	^ socket notNil and: [ self socketIsConnected ]