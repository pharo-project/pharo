testing
testPlainClientRead
	| dataSent dataRead clientStream semaphore |
	dataSent := #[ 6 5 4 3 2 1 ].
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent = dataRead.
	semaphore wait