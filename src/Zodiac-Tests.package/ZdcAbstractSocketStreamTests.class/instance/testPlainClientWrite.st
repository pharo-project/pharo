testing
testPlainClientWrite
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		dataRead := serverStream upToEnd.
		serverStream close.
		mySemaphore signal ].
	dataSent := #[ 1 2 3 4 5 6 ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	clientStream nextPutAll: dataSent.
	clientStream close.
	semaphore wait.
	self assert: dataSent = dataRead.
	semaphore wait