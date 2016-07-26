tests
testLocalAddress
	"Tests the various localAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket localAddress asSocketAddress = self listenerAddress.
	self assert: clientSocket localAddress asSocketAddress = self listenerAddress.
	self assert: serverSocket localAddress asSocketAddress = self listenerAddress.
