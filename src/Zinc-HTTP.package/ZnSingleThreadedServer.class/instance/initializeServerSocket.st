private
initializeServerSocket
	"Set up a new server socket and listen on it"
	
	serverSocket := self bindingAddress
		ifNil: [ ZnNetworkingUtils serverSocketOn: self port ]
		ifNotNil: [ ZnNetworkingUtils serverSocketOn: self port interface: self bindingAddress ].
	self logServerSocketBound