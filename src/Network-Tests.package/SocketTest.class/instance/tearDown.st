running
tearDown

	listenerSocket ifNotNil:[listenerSocket destroy].
	clientSocket ifNotNil:[clientSocket destroy].
	serverSocket ifNotNil:[serverSocket destroy].
