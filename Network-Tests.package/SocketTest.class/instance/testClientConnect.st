tests
testClientConnect
	"Tests a client socket connection"

	clientSocket := Socket newTCP.
	clientSocket connectTo: self listenerAddress port: self listenerPort.
	clientSocket waitForConnectionFor: 2.
	self assert: clientSocket isConnected.
