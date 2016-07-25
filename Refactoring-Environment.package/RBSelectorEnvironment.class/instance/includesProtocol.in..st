testing
includesProtocol: aProtocol in: aClass 
	^(super includesProtocol: aProtocol in: aClass)
		and: [(environment selectorsFor: aProtocol in: aClass)
				anySatisfy: [:aSelector | self privateIncludesSelector: aSelector inClass: aClass]]