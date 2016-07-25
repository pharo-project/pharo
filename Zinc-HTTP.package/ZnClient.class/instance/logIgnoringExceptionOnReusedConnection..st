private logging
logIgnoringExceptionOnReusedConnection: exception
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientIgnoringExceptionOnConnectionReuseEvent)
		exception: exception;
		emit