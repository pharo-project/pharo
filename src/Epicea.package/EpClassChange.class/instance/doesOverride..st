testing
doesOverride: aCodeChange

	^ aCodeChange isEpClassChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]