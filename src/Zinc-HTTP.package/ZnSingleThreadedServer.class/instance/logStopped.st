private logging
logStopped
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerStoppedEvent)
		description: self serverProcessName;
		emit