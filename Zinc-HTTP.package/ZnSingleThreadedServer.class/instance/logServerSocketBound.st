private logging
logServerSocketBound
	logLevel < 1 ifTrue: [ ^ nil ].
	^ (self newLogEvent: ZnServerSocketBoundEvent)
		address: ([ serverSocket address ] on: Error do: [ nil ]);
		port: ([ serverSocket port ] on: Error do: [ nil ]);
		emit