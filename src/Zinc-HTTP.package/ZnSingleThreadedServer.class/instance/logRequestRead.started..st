private logging
logRequestRead: request started: initialMilliseconds
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnRequestReadEvent)
				request: request;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]