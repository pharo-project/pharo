visitor
visitProtocolRemoval: aProtocolChange
	^ self 
		displayClass: aProtocolChange behaviorAffectedName
		protocol: aProtocolChange protocol asString