visitor
visitProtocolRemoval: aProtocolRemoved

	self 
		behaviorNamed: aProtocolRemoved behaviorAffectedName
		ifPresent: [ :behavior | 
			^ behavior organization protocolOrganizer hasProtocolNamed: aProtocolRemoved protocol ].
		
	^ true