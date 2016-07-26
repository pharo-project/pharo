visitor
visitProtocolRemoval: aProtocolRemoved

	self 
		behaviorNamed: aProtocolRemoved behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeProtocol: aProtocolRemoved protocol ]