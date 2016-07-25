ui requests
syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification
	
	"display and log the syntax error"
	super syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification.
	
	"in noninteractive mode simply quit"
	^ self exitFailure