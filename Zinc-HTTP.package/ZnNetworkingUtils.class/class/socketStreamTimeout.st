constants
socketStreamTimeout
	"Access the current timeout in seconds for SocketStream IO"
	
	^ ZnConnectionTimeout value 
		ifNil: [ self defaultSocketStreamTimeout ]