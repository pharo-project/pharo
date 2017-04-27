testing
doesOverride: aCodeChange

	^ aCodeChange isEpTraitChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]