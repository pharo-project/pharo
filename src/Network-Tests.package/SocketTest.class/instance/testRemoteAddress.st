tests
testRemoteAddress
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remoteAddress = #[0 0 0 0].
	self assert: clientSocket remoteAddress asSocketAddress = self listenerAddress.
	self assert: serverSocket remoteAddress asSocketAddress = self listenerAddress.
