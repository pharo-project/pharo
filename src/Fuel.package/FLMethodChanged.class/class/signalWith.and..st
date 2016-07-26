signaling
signalWith: aGlobalName and: aSelector
	^ self signal: 'Method ', aGlobalName, '>>#', aSelector, ' changed its bytecodes.'