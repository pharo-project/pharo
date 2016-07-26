private logging
logServerReadError: exception
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerReadErrorEvent)
		exception: exception;
		emit