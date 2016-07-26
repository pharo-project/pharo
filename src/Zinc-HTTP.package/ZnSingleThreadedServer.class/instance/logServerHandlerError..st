private logging
logServerHandlerError: exception
	^ (self newLogEvent: ZnServerHandlerErrorEvent)
		exception: exception;
		emit