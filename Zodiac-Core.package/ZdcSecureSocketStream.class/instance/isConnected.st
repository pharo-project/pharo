testing
isConnected
	"Are we connected at the socket level ?
	Has the SSL handshake been done successfully ?"

	^ super isConnected 
		and: [ sslSession notNil and: [ connecting or: [ sslSession isConnected ] ] ]