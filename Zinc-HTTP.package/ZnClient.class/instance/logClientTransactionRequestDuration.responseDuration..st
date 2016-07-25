private logging
logClientTransactionRequestDuration: requestDuration responseDuration: responseDuration
	| logEventClass |
	logLevel < 1 ifTrue: [ ^ nil ].
	logEventClass := logLevel = 1
		ifTrue: [ ZnSimplifiedClientTransactionEvent ]
		ifFalse: [ ZnClientTransactionEvent ].
	^ (self newLogEvent: logEventClass)
		request: request;
		requestDuration: requestDuration;
		response: response; 
		responseDuration: responseDuration;
		emit;
		yourself