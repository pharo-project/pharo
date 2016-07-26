private socket
socketIsConnected
	^ socket isConnected and: [ socket isOtherEndClosed not ]