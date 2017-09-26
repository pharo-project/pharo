running
setUp
	| listener clientSocket serverSocket |
	listener := Socket newTCP.
	[listener listenOn: 0 backlogSize: 4.

	clientSocket := Socket newTCP.
	clientSocket connectTo: #[127 0 0 1] port: listener localPort.
	clientSocket waitForConnectionFor: 1.
	self assert: clientSocket isConnected.

	serverSocket := listener waitForAcceptFor: 1.
	self assert: serverSocket isConnected.
	] ensure:[listener destroy].

	clientStream := SocketStream on: clientSocket.
	serverStream := SocketStream on: serverSocket.
