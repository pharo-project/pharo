accessing
protocolNamed: aName

	^ self 
		protocolNamed: aName 
		ifAbsent: [ Protocol empty ]