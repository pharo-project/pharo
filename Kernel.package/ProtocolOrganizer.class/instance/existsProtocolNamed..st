private
existsProtocolNamed: aProtocolName
	
	^self allProtocols anySatisfy: [ :e | e name = aProtocolName ] 