accessing
protocolsFor: aClass
	^aClass organization categories 
		select: [ :each | self includesProtocol: each in: aClass ]