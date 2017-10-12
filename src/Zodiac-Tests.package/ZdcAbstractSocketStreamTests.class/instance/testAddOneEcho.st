tests
testAddOneEcho
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream data |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream next.
		stream nextPut: ((data + 1) bitAnd: 16rff).  
		stream close ].
	dataSent := 99.
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPut: dataSent; flush.
	dataRead := clientStream next.
	clientStream close.
	self assert: dataRead = (dataSent + 1).
	semaphore wait