testing
includesProtocol: aProtocol in: aClass
	^ (environment includesProtocol: aProtocol in: aClass)
		or: [ orEnvironment includesProtocol: aProtocol in: aClass ]