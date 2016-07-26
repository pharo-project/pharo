testing
clientSend: message
	"Send message to the local TCP echo service and return the result"
	
	| socket |
	socket := Socket newTCP.
	^ [ 
		socket connectTo: self localhost port: self port.
		socket sendData: message.
		socket receiveData ] 
			ensure: [ socket closeAndDestroy ]