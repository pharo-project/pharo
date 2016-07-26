testing
testLocalHostAddress
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostAddressIsLoopBackAddress]
		ifFalse: [self assert: self localHostAddressIsLoopBackAddress]