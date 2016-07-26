testing
testPlainClientRead10kInPieces1
	| dataSent dataRead clientStream semaphore |
	dataSent := self bytes: 10000.
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := ByteArray new: 10000 streamContents: [ :stream |
		10 timesRepeat: [ stream nextPutAll: (clientStream next: 1000) ] ].
	clientStream close.
	semaphore wait.
	self assert: dataSent = dataRead.
	semaphore wait