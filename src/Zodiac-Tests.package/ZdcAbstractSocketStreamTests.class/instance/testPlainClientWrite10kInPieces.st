tests - plain
testPlainClientWrite10kInPieces
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		dataRead := serverStream upToEnd.
		serverStream close.
		mySemaphore signal ].
	dataSent := self bytes: 10000.
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	0 to: 9 do: [ :index |
		clientStream next: 1000 putAll: dataSent startingAt: (1000 * index) + 1 ].
	clientStream close.
	semaphore wait.
	self assert: dataSent = dataRead.
	semaphore wait