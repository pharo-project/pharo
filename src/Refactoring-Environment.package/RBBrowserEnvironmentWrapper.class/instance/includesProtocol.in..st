testing
includesProtocol: aProtocol in: aClass
	^(self includesClass: aClass)
		and: [environment includesProtocol: aProtocol in: aClass]