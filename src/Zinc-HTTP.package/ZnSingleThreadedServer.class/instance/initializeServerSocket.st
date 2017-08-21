private
initializeServerSocket
	"Set up a new server socket and listen on it"
	
	serverSocket := self bindingAddress
		ifNil: [ ZnNetworkingUtils serverSocketOn: self port ]
		ifNotNil: [ ZnNetworkingUtils serverSocketOn: self port interface: self bindingAddress ].
	"Binding a server socket to port 0 means letting the OS assign a port, fetch it"
	self port isZero 
		ifTrue: [ self optionAt: #port put: serverSocket port ].
	self logServerSocketBound