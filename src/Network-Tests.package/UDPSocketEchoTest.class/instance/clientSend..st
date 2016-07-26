testing
clientSend: message
	"Send message to the local UDP echo service and return the result"
	
	| socket |
	socket := Socket newUDP.
	^ [ 
		socket sendUDPData: message toHost: self localhost port: self port.
		self receiveUDPDataFrom: socket ] 
			ensure: [ socket closeAndDestroy ]