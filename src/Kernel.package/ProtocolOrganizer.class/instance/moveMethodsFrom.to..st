private
moveMethodsFrom: fromProtocolNamed to: toProtocolNamed
	| fromProtocol toProtocol |
	
	fromProtocol := self protocolNamed: fromProtocolNamed.
	toProtocol := self protocolNamed: toProtocolNamed.
	
	toProtocol addAllMethodsFrom: fromProtocol.
	fromProtocol removeAllMethods.
	
	^ toProtocol.