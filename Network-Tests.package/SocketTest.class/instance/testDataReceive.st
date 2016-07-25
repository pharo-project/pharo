tests
testDataReceive
	"Test data transfer and related methods"

	self testDataSending.
	"It can take a tad for the status change to be visible"
	(Delay forMilliseconds: 200) wait.
	self assert: serverSocket dataAvailable.
	self assert: (serverSocket receiveData = 'Hello World').
	self deny: (serverSocket dataAvailable).
