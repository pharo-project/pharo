testing
isConnected
	"Return true if I hold an open HTTP connection."

	^ connection notNil and: [ connection isConnected ]