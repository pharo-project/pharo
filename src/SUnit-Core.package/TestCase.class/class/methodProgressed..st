testing
methodProgressed: aSelector
	^ ((self storedMethodRaisedError: aSelector) or: [self storedMethodFailed: aSelector])
		and: [self methodPassed: aSelector]
		