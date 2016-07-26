visitor
visitProtocolAddition: aProtocolChange
	^ self 
		displayClass: aProtocolChange behaviorAffectedName
		protocol: aProtocolChange protocol asString