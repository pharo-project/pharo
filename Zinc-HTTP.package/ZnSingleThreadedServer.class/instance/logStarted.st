private logging
logStarted
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerStartedEvent)
		description: self serverProcessName;
		emit