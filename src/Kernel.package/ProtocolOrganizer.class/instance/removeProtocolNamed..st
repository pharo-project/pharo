protocol - removing
removeProtocolNamed: aName
	| protocolToRemove |
	
	protocolToRemove := self protocolNamed: aName.
	^ self removeProtocol: protocolToRemove