testing
includesProtocol: aProtocol in: aClass
	^ (environment includesProtocol: aProtocol in: aClass) and: [ (self selectorsFor: aProtocol in: aClass) notEmpty ]