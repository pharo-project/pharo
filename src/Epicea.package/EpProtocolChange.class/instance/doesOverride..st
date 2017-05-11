testing
doesOverride: aCodeChange 

	^ aCodeChange isEpProtocolChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName and: [ 
		aCodeChange protocol = self protocol ] ]