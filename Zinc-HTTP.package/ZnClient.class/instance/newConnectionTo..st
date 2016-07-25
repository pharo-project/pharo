private
newConnectionTo: url
	| initialMilliseconds |
	self signalHTTPProgress: 'Connecting to ',  url authority.
	initialMilliseconds := Time millisecondClockValue.
	(connection notNil and: [ connection isConnected ]) 
		ifTrue: [ connection close ].
	connection := ZnNetworkingUtils socketStreamToUrl: url.
	url hasSecureScheme
		ifTrue: [ self setupTLSTo: url ].
	self logConnectionEstablishedTo: url started: initialMilliseconds