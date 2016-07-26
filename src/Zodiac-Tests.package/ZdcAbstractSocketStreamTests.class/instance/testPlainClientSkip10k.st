testing
testPlainClientSkip10k
	| dataSent dataRead clientStream semaphore |
	dataSent := #[ 6 5 4 3 2 1 ].
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		10000 timesRepeat: [ serverStream nextPut: 0 ].  
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream skip: 10000; upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent = dataRead.
	semaphore wait