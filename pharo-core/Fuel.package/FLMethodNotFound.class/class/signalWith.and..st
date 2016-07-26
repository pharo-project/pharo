signaling
signalWith: aGlobalName and: aSelector
	^ self signal: 'Method ', aGlobalName, '>>#', aSelector, ' not found.'