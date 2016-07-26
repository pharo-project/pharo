private logging
logConnectionClosedAddress: address port: port
	logLevel < 3 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnClientConnectionClosedEvent)
		address: address;
		port: port;
		emit