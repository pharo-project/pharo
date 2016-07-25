private logging
logResponseReadStarted: initialMilliseconds
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnResponseReadEvent)
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]