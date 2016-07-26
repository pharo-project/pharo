testing
includesProtocol: aProtocol in: aClass 
	^ (environment includesProtocol: aProtocol in: aClass)
		and: [ andedEnvironment includesProtocol: aProtocol in: aClass ]