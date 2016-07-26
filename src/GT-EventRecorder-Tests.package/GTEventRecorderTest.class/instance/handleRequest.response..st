server handling
handleRequest: request response: response
	receivedContents nextPut: request contents.
	receiverSemaphore signal.
	^ response