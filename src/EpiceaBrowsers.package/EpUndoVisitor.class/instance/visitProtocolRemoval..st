visitor
visitProtocolRemoval: aProtocolRemoved
	self 
		behaviorNamed: aProtocolRemoved behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior organization addCategory: aProtocolRemoved protocol ]