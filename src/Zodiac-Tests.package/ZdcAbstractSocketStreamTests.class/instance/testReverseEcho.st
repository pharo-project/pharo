testing
testReverseEcho
	| dataSent dataRead clientStream semaphore data |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream upTo: 0.
		stream nextPutAll: data reverse; nextPut: 0.  
		stream close ].
	dataSent := #[ 1 2 3 4 5 6 ].
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPutAll: dataSent; nextPut: 0; flush.
	dataRead := clientStream upTo: 0.
	clientStream close.
	self assert: dataRead = dataSent reverse.
	semaphore wait