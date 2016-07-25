testing
methodRegressed: aSelector
	^ (self storedMethodPassed: aSelector) and: [(self methodFailed: aSelector) or: [self methodRaisedError: aSelector]]