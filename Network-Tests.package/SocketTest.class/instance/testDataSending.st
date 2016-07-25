tests
testDataSending
	"Test data transfer and related methods"

	self testServerAccept.
	clientSocket sendData: 'Hello World'.
	clientSocket waitForSendDoneFor: 2.
	self assert: clientSocket sendDone.

