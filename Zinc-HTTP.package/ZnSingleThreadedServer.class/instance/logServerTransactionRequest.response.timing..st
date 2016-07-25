private logging
logServerTransactionRequest: request response: response timing: timing
	| logEventClass |
	logLevel < 1 ifTrue: [ ^ nil ].
	logEventClass := logLevel = 1
		ifTrue: [ ZnSimplifiedServerTransactionEvent ]
		ifFalse: [ ZnServerTransactionEvent ].
	^ (self newLogEvent: logEventClass)
		request: request;
		response: response;
		timing: timing;
		emit