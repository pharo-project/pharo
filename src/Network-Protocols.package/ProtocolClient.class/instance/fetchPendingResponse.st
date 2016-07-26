private protocol
fetchPendingResponse
	^pendingResponses
		ifNil: [self fetchNextResponse; lastResponse]
		ifNotNil: [self popResponse]