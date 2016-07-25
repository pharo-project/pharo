menus
loadProtocolSelection
	protocolSelection ifNil: [ ^self ].
	self methodsForSelectedProtocol do: [ :m | m load ].