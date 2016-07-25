testing
includesProtocol: aProtocol in: aClass 
	^(aClass organization protocolOrganizer getProtocolNamed: aProtocol ifNone: [ ^ false ])
		methods anySatisfy: [ :selector | 
			self includesSelector: selector in: aClass ]