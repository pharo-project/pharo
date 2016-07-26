testing
testLocalHostName
	self deny: NetNameResolver localHostName isNil.
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostNameIsLoopBackName]
		ifFalse: [self assert: self localHostNameIsLoopBackName]