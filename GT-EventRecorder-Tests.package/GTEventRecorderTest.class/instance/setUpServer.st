running
setUpServer
	receivedContents := WaitfreeQueue new.
	receiverSemaphore := Semaphore new.
	delegate := ZnDispatcherDelegate new.
	delegate
		map: '/'
		to: [ :request :response | self handleRequest: request response: response ].
	server := GTEventRecorderTestServer new.
	server initializeUsingDelegate: delegate semaphore: receiverSemaphore.