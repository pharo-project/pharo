private logging
logConnectionAccepted: socket
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnConnectionAcceptedEvent)
		address: ([ socket remoteAddress ] on: Error do: [ nil ]);
		emit