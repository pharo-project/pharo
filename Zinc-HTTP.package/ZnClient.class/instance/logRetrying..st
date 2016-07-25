private logging
logRetrying: exception
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientRetryingEvent)
		exception: exception;
		emit