tests
testServerAccept
	"Tests a server-side accept"

	self testClientConnect.
	serverSocket := listenerSocket waitForAcceptFor: 2.
	self assert: (serverSocket notNil).
	self assert: (serverSocket isConnected).
