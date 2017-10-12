tests - reverse
testReverseEchoUpToEnd
	| dataSent dataRead clientStream semaphore data |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket stream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		"Doing #upToEnd here does not seem to work"
		data := stream next: 6.
		stream nextPutAll: data reverse.  
		stream close ].
	self runClient: [ 
		dataSent := #[ 1 2 3 4 5 6 ].
		clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
		clientStream nextPutAll: dataSent; flush.
		dataRead := clientStream upToEnd.
		clientStream close.
		self assert: dataRead = dataSent reverse.
		semaphore wait ]