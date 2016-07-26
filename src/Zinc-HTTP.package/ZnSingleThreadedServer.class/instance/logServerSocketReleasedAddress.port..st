private logging
logServerSocketReleasedAddress: address port: port
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerSocketReleasedEvent)
		address: address;
		port: port;
		emit