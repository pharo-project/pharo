tests
testLocalPort
	"Tests the various localPort values for sockets"

	self testServerAccept.
	self assert: listenerSocket localPort = self listenerPort.
	self assert: clientSocket localPort > 0.
	self assert: serverSocket localPort > 0.
