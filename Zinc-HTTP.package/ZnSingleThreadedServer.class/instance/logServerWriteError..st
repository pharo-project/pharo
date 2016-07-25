private logging
logServerWriteError: exception
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerWriteErrorEvent)
		exception: exception;
		emit