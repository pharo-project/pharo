private logging
logRequest: request response: response handledStarted: initialMilliseconds
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnRequestResponseHandledEvent)
				request: request;
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]