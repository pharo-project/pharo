backward compatibility
classify: selector under: aProtocolName suppressIfDefault: aBoolean
	| oldProtocol forceNotify |
	forceNotify := (self protocolOrganizer includesSelector: selector) not.
	oldProtocol := self categoryOfElement: selector.
	(forceNotify or: [ oldProtocol ~= aProtocolName or: [ aBoolean not or: [ aProtocolName ~= Protocol unclassified ] ] ])
		ifFalse: [ ^ self ].
	self protocolOrganizer classify: selector inProtocolNamed: aProtocolName suppressIfDefault: aBoolean.
	oldProtocol ifNotNil: [ self notifyOfChangedSelector: selector from: oldProtocol to: aProtocolName ]