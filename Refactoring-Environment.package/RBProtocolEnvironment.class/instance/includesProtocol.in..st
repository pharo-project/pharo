testing
includesProtocol: aProtocol in: aClass
	^aClass == class and: [(super includesProtocol: aProtocol in: aClass)
			and: [protocols includes: aProtocol]]