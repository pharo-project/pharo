backward compatibility
classify: selector under: aProtocolName suppressIfDefault: aBoolean
	| oldProtocol |
	oldProtocol := self protocolOrganizer protocolOfSelector: selector. 
	(oldProtocol isNil or: [ oldProtocol name ~= aProtocolName or: [ aBoolean not or: [ aProtocolName ~= Protocol unclassified ] ] ])
		ifFalse: [ ^ self ].
	self protocolOrganizer classify: selector inProtocolNamed: aProtocolName.
	oldProtocol ifNotNil: [ 
		aBoolean & oldProtocol canBeRemoved ifTrue: [ self removeProtocol: oldProtocol ].
		self notifyOfChangedSelector: selector from: oldProtocol name to: aProtocolName ]