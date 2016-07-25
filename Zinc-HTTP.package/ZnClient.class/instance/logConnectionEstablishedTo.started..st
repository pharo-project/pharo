private logging
logConnectionEstablishedTo: url started: initialMilliseconds
	| event |
	logLevel < 3 ifTrue: [ ^ nil ].
	(event := self newLogEvent: ZnConnectionEstablishedEvent)
		hostname: url host;
		port: url portOrDefault;
		address: ([ connection socket remoteAddress ] on: Error do: [ nil ]);
		duration: (Time millisecondsSince: initialMilliseconds).
	(ZnNetworkingUtils shouldProxyUrl: url)
		ifTrue: [ event proxy: ZnNetworkingUtils proxyUrl ].
	event emit