testing
hasProtocolNamed: aString

	^ self allProtocols anySatisfy: [ :each | each name = aString ]