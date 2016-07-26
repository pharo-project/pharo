private logging
logResponseWritten: response started: initialMilliseconds
	^ logLevel < 3
		ifTrue: [ Time millisecondsSince: initialMilliseconds ]
		ifFalse: [ 
			(self newLogEvent: ZnResponseWrittenEvent)
				response: response;
				duration: (Time millisecondsSince: initialMilliseconds);
				emit;
				duration ]