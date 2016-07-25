tests
testRemotePort
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remotePort = 0.
	self assert: clientSocket remotePort = self listenerPort.
	self assert: serverSocket remotePort > 0.
