testing
doesOverride: aCodeChange

	^ aCodeChange isEpMethodChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName and: [ 
		aCodeChange methodAffectedSelector = self methodAffectedSelector ] ]