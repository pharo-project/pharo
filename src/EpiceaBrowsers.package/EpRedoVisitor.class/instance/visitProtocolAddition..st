visitor
visitProtocolAddition: aProtocolAddition
	self 
		behaviorNamed: aProtocolAddition behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior organization addCategory: aProtocolAddition protocol ]