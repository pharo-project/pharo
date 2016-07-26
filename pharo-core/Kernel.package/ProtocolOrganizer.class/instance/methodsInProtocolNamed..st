backward compatibility
methodsInProtocolNamed: aName
	aName = AllProtocol defaultName 
		ifTrue: [ ^ self allMethods ].

	^ (self protocolNamed: aName) methods