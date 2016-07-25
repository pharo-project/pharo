testing
includesProtocol: aProtocol in: aClass
	^ (environment includesProtocol: aProtocol in: aClass) and: [ self packages anySatisfy: [ :package | package includesMethodCategory: aProtocol ofClass: aClass ] ]