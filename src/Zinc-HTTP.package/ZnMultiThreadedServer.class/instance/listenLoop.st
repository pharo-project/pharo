request handling
listenLoop
	"We create a listening Socket, then wait for a connection.
	After each connection we also check that the listening Socket is still valid 
	- if not we just make a recursive call to this method to start over."

	self initializeServerSocket.
	[ [ 
		serverSocket isValid 
			ifFalse: [
				"will trigger #ifCurtailed: block and destroy socket"
				^ self listenLoop ].
		self serveConnectionsOn: serverSocket ] repeat ]

		ifCurtailed: [ self releaseServerSocket ]