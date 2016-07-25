protocol
classify: aSymbol inProtocolNamed: aProtocolName suppressIfDefault: aBoolean
	| oldProtocols |
	
	oldProtocols := self protocolsOfSelector: aSymbol.

	self 
		classify: aSymbol 
		inProtocolNamed: aProtocolName.
		
	aBoolean ifTrue: [
		(oldProtocols 
			select: #canBeRemoved)
			do: [:e | self removeProtocol: e ] ].