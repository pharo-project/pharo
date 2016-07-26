accessing
removeCategory: protocolName
	(self protocolOrganizer hasProtocolNamed: protocolName)
		ifFalse: [ ^ self ].
	
	self removeProtocol: (self protocolNamed: protocolName)