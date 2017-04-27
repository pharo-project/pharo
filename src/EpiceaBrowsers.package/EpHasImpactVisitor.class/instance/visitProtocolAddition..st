visitor
visitProtocolAddition: aProtocolAddition

	self 
		behaviorNamed: aProtocolAddition behaviorAffectedName
		ifPresent: [ :behavior |
			^ (behavior organization protocolOrganizer hasProtocolNamed: aProtocolAddition protocol) not ].
		
	^ true