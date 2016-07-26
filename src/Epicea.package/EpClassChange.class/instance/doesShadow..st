testing
doesShadow: aCodeChange

	^ aCodeChange isEpClassChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]