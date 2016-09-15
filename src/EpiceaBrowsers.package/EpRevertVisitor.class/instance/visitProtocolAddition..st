visitor
visitProtocolAddition: aProtocolAddition
	self 
		behaviorNamed: aProtocolAddition behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeProtocol: aProtocolAddition protocol ]