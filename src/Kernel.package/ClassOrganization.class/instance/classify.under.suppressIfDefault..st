backward compatibility
classify: selector under: aProtocolName suppressIfDefault: aBoolean
	| oldCategory forceNotify oldProtocols |
	forceNotify := (self protocolOrganizer includesSelector: selector) not.
	oldCategory := self categoryOfElement: selector.
	(forceNotify or: [ oldCategory ~= aProtocolName or: [ aBoolean not or: [ aProtocolName ~= Protocol unclassified ] ] ])
		ifFalse: [ ^ self ].
	oldProtocols := self protocolOrganizer protocolsOfSelector: selector.
	self protocolOrganizer
		classify: selector
		inProtocolNamed: aProtocolName.		
	aBoolean ifTrue: [
		(oldProtocols 
			select: #canBeRemoved)
			do: [:e | self  removeProtocol: e ] ].
	oldCategory ifNotNil: [ self notifyOfChangedSelector: selector from: oldCategory to: aProtocolName ]